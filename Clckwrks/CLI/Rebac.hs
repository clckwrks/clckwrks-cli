{-# language OverloadedStrings #-}
module Clckwrks.CLI.Rebac where

import AccessControl.Relation (RelationTuple(..), pRelationTuple, ppRelationTuple, ppRelationTuples)
import Control.Applicative ((<$>), (<*>), (*>), pure)
import Clckwrks (UserId(..))
import Clckwrks.CLI.Core (CLIHandler(..), Parser)
import Clckwrks.Rebac.Acid (AddRelationTuple(..), RebacState, GetRelationTuples(..), GetRelationLog(..), RLEAction(..), RelationLogEntry(..), RemoveRelationTuple(..))
import Control.Monad.Reader
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Text (Text)
import qualified Data.Text as T
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
      deriving (Eq, Ord, Read, Show)

showRebacHelp :: [ String ]
showRebacHelp =
  [ "rebac relation-log                               - show entire relation log (will eventually get very long)"
  , "rebac relations                                  - show all currently active relation tuples"
  , "rebac add-relation    <relation-tuple> <comment> - add a new relation tuple"
  , "rebac remove-relation <relation-tuple> <comment> - remove a relation tuple"
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
       ]

ppRelationLogEntry (RelationLogEntry timestamp relationTuple action comment) =
  PP.text (show timestamp) <+> ppAction action <+> ppRelationTuple relationTuple <+> PP.text (T.unpack comment)
  where
    ppAction RLEAdd    = PP.text "+"
    ppAction RLERemove = PP.text "-"

ppRelationLogEntries :: [ RelationLogEntry ] -> Doc
ppRelationLogEntries entries = PP.vcat $ map ppRelationLogEntry entries

execRebacCommand :: RebacCmd -> ReaderT (AcidState RebacState) IO ()
execRebacCommand RCRelationLog =
  do a <- ask
     rl <- query' a GetRelationLog
     liftIO $ print $ ppRelationLogEntries rl
execRebacCommand RCRelationTuples =
  do a <- ask
     rt <- query' a GetRelationTuples
     liftIO $ print $ ppRelationTuples rt
execRebacCommand (RCAddRelationTuple rt comment) =
  do a <- ask
     now <- liftIO getCurrentTime
     e <- update' a (AddRelationTuple rt now comment)
     liftIO $ print $ ppRelationLogEntry e
execRebacCommand (RCRemoveRelationTuple rt comment) =
  do a <- ask
     now <- liftIO getCurrentTime
     e <- update' a (RemoveRelationTuple rt now comment)
     liftIO $ print $ ppRelationLogEntry e


initRebacCommand :: FilePath -> IO (RebacCmd -> IO ())
initRebacCommand basePath =
    do rebac <- openRemoteStateSockAddr skipAuthenticationPerform (SockAddrUnix ((basePath </> "rebac_socket")))
       pure $ \c -> runReaderT (execRebacCommand c) rebac

rebacCLIHandler :: FilePath -> IO CLIHandler
rebacCLIHandler basePath =
  do exec <- initRebacCommand basePath
     pure $ CLIHandler
       { cliPrefix = "rebac"
       , cliExec   = exec
       , cliParser = pRebacCmd
       , cliHelp   = showRebacHelp
       }
