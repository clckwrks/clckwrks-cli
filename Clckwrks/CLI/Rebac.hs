{-# language DataKinds, OverloadedStrings #-}
module Clckwrks.CLI.Rebac where

import AccessControl.Check (RelPerm, check, lookupSubjects, lookupSubjectsWithType, mkDefMap)
import AccessControl.Relation ( Relation, RelationTuple(..), Object(..), ObjectType(..), ObjectWildcard(..)
                              , hasRelation, hasResource, hasResourceType, hasSubject, hasSubjectType
                              , pObject, pObjectType, pObjectWild, pRelation, pRelationTuple
                              , ppRelationTuple, ppRelationTuples
                              )
import AccessControl.Schema (Schema(definitions), Permission(..), parseSchema, pPermission)
import Control.Applicative ((<$>), (<*>), (*>), pure)
import Clckwrks (UserId(..))
import Clckwrks.CLI.Core (CLIHandler(..), Parser)
import Clckwrks.Rebac.Acid (AddRelationTuple(..), RebacState, GetRelationTuples(..), GetRelationLog(..), RLEAction(..), RelationLogEntry(..), RemoveRelationTuple(..))
import Control.Monad.Reader
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import qualified Data.ByteString    as BS
import           Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock              (getCurrentTime)
import Network.Socket (SockAddr(..))
import Data.Acid.Remote (openRemoteStateSockAddr, skipAuthenticationPerform)
import qualified Data.Text as T
import System.Environment
import System.FilePath ((</>))
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($$), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

data RebacCmd
    = RCRelationLog
    | RCRelationTuples
    | RCAddRelationTuple RelationTuple Text
    | RCRemoveRelationTuple RelationTuple Text
    | RCCheck (Object NoWildcard) Permission (Object NoWildcard) -- ^ resource, permission, subject
    | RCHasSubjectType ObjectType
    | RCHasSubject (Object AllowWildcard)
    | RCHasResourceType ObjectType
    | RCHasResource (Object NoWildcard)
    | RCHasRelation Relation
    | RCHasResourceTypeRelation ObjectType Relation
    | RCHasResourceRelation (Object NoWildcard) Relation
    | RCHasResourceTypeSubject ObjectType (Object AllowWildcard)
      deriving (Eq, Ord, Read, Show)

showRebacHelp :: [ String ]
showRebacHelp =
  [ "rebac relation-log                               - show entire relation log (will eventually get very long)"
  , "rebac relations                                  - show all currently active relation tuples"
  , "rebac add-relation    <relation-tuple> <comment> - add a new relation tuple"
  , "rebac remove-relation <relation-tuple> <comment> - remove a relation tuple"
  , "rebac check <resourceType:resourceId> <permission> <subjectType:subjectId>    - check if a subject has permission on resource"
  , "rebac has-subject-type <subjectType>             - show all relations with the specified subject type"
  , "rebac has-subject <subjectType:subjectId>        - show all relation tuples with the specified subject"
  , "rebac has-resource-type <resourceType>           - show all relation tuples with the specified resource type"
  , "rebac has-resource <resourceType:resourceId>     - show all relation tuples with the specified resource"
  , "rebac has-relation <relation>     - show all relation tuples with the specified relation"
  , "rebac has-resource-type-relation <resourceType> <relation> - show all subjects for the specific resource type & relation"
  , "rebac has-resource-relation <resourceType:resourceId> <relation> - show all subjects for the specific resource & relation"
  , "rebac has-resource-type-subject <resourceType> <subjectType:subjectId> - show all tuples for the specific resource type & subject"
  ]

pComment :: Parser Text
pComment =  T.pack <$> (hspace *> some printChar)

pRebacCmd :: Parser RebacCmd
pRebacCmd =
  msum [ do string "relation-log"
            pure RCRelationLog
       , do string "relations"
            pure RCRelationTuples
       , do string "add-relation"
            hspace
            rt <- pRelationTuple
            comment <- pComment
            pure (RCAddRelationTuple rt comment)
       , do string "remove-relation"
            hspace
            rt <- pRelationTuple
            comment <- pComment
            pure (RCRemoveRelationTuple rt comment)
       , do string "check"
            hspace
            resource <- pObject
            hspace
            perm <- pPermission
            hspace
            subject <- pObject
            pure (RCCheck resource perm subject)
       , do string "has-subject-type"
            hspace
            st <- pObjectType
            pure (RCHasSubjectType st)
       , do string "has-subject"
            hspace
            sbj <- pObjectWild
            pure (RCHasSubject sbj)
       , try $
          do string "has-resource-type"
             hspace
             rt <- pObjectType
             pure (RCHasResourceType rt)
       , try $
          do string "has-resource-type-subject"
             hspace
             rt <- pObjectType
             hspace
             subject <- pObjectWild
             pure (RCHasResourceTypeSubject rt subject)
       , try $
          do string "has-resource"
             hspace
             res <- pObject
             pure (RCHasResource res)
       , do try $ string "has-resource-type-relation"
            hspace
            resTy <- pObjectType
            hspace
            rel <- pRelation
            pure (RCHasResourceTypeRelation resTy rel)
       , do string "has-resource-relation"
            hspace
            res <- pObject
            hspace
            rel <- pRelation
            pure (RCHasResourceRelation res rel)
       , do string "has-relation"
            hspace
            rel <- pRelation
            pure (RCHasRelation rel)
       ]

ppRelationLogEntry (RelationLogEntry timestamp relationTuple action comment) =
  PP.text (show timestamp) <+> ppAction action <+> ppRelationTuple relationTuple <+> PP.text (T.unpack comment)
  where
    ppAction RLEAdd    = PP.text "+"
    ppAction RLERemove = PP.text "-"

ppRelationLogEntries :: [ RelationLogEntry ] -> Doc
ppRelationLogEntries entries = PP.vcat $ map ppRelationLogEntry entries

execRebacCommand :: RebacCmd -> Maybe (Map T.Text RelPerm) -> ReaderT (AcidState RebacState) IO ()
execRebacCommand RCRelationLog _ =
  do a <- ask
     rl <- query' a GetRelationLog
     liftIO $ print $ ppRelationLogEntries rl
execRebacCommand RCRelationTuples _ =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples rt
execRebacCommand (RCAddRelationTuple rt comment) _ =
  do a <- ask
     now <- liftIO getCurrentTime
     e <- update' a (AddRelationTuple rt now comment)
     liftIO $ print $ ppRelationLogEntry e
execRebacCommand (RCRemoveRelationTuple rt comment) _ =
  do a <- ask
     now <- liftIO getCurrentTime
     e <- update' a (RemoveRelationTuple rt now comment)
     liftIO $ print $ ppRelationLogEntry e
execRebacCommand (RCCheck resource perm subject) Nothing =
  do liftIO $ print "No schema file was specified -- unable to do anything"
execRebacCommand (RCCheck resource perm subject) (Just rsDefMap) =
  do a <- ask
     rts <- query' a GetRelationTuples
     let access = check rsDefMap rts resource perm subject
     liftIO $ print access
execRebacCommand (RCHasSubjectType st) _ =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (hasSubjectType st) rt
execRebacCommand (RCHasSubject sbj) _ =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (hasSubject sbj) rt
execRebacCommand (RCHasResourceType resTy) _ =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (hasResourceType resTy) rt
execRebacCommand (RCHasResourceTypeSubject resTy subject) _ =
  do a <- ask
     rts <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (\rt -> hasResourceType resTy rt && hasSubject subject rt) rts
execRebacCommand (RCHasResource res) _ =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (hasResource res) rt
execRebacCommand (RCHasRelation rel) _ =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (hasRelation rel) rt
execRebacCommand (RCHasResourceRelation res rel) _ =
  do a <- ask
     rts <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (\rt -> hasRelation rel rt && hasResource res rt) rts
execRebacCommand (RCHasResourceTypeRelation resTy rel) _ =
  do a <- ask
     rts <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples $ filter (\rt -> hasRelation rel rt && hasResourceType resTy rt) rts

initRebacCommand :: FilePath -> Maybe FilePath -> IO (RebacCmd -> IO ())
initRebacCommand basePath mSchemaPath =
    do rebac <- openRemoteStateSockAddr skipAuthenticationPerform (SockAddrUnix ((basePath </> "rebac_socket")))
       mRsDefMap <-
         case mSchemaPath of
           Nothing -> pure Nothing
           (Just schemaPath) ->
             do c <- BS.readFile schemaPath
                pure $ case parseSchema $ T.decodeUtf8 $ c of
                         (Left e)  -> error e
                         (Right s) -> Just $ mkDefMap (definitions s)
       pure $ \c -> runReaderT (execRebacCommand c mRsDefMap) rebac

rebacCLIHandler :: FilePath -> Maybe FilePath -> IO CLIHandler
rebacCLIHandler basePath mSchema =
  do exec <- initRebacCommand basePath mSchema
     pure $ CLIHandler
       { cliPrefix = "rebac"
       , cliExec   = exec
       , cliParser = pRebacCmd
       , cliHelp   = showRebacHelp
       }
