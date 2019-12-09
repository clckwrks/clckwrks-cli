{-# language ExistentialQuantification #-}
module Clckwrks.CLI.Core where

import Control.Applicative ((<$>), (<*>), (*>), pure)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Remote (openRemoteState, skipAuthenticationPerform)
import           Data.Map (Map)
import qualified Data.Map as Map
import System.Environment
import System.Exit (exitSuccess)
import System.Console.Haskeline
import Text.Parsec
import Text.Parsec.String

data CLIHandler = forall cmd. CLIHandler
  { cliPrefix :: String
  , cliExec   :: cmd -> IO ()
  , cliParser :: Parser cmd
  , cliHelp   :: [String]
  }

data Command
    = Quit
    | Help
      deriving (Eq, Ord, Read, Show)

pCommand :: Parser Command
pCommand =
    do string "quit" *> return Quit
    <|>
    do string "help" *> return Help

{-
    do string "user"
       skipMany1 space
       User <$> pUserCmd
    <|>
-}


loop :: [CLIHandler] -> IO () -- InputT IO ()
loop handlers' =
  let handlers = Map.fromList (map (\h -> (cliPrefix h, h)) handlers') in
  runInputT defaultSettings $ forever $
  do minput <- getInputLine "% "
     case minput of
       Nothing -> return ()
       Just input ->
         let (prefix, rest') = span (/= ' ') $ dropWhile (== ' ') $ input
             rest = dropWhile (== ' ') $ rest'
         in case prefix of
           "help" -> liftIO $ execCommand (concatMap (cliHelp . snd) (Map.toAscList handlers)) Help
           "quit" -> liftIO $ exitSuccess
           _ -> case Map.lookup prefix handlers of
                  Nothing -> liftIO $ putStrLn $ "unknow command prefix: " ++ prefix
                  (Just (CLIHandler _ exec parser _)) ->
                    do let r = parse parser input rest
                       case r of
                         (Left e) ->
                           do liftIO $ print e
                         (Right cmd) ->
                           do liftIO $ exec cmd

execCommand :: [String] -> Command -> IO ()
execCommand helps Help =
    do putStrLn $ unlines $ (helps ++ showHelp)
       return ()

showHelp :: [String]
showHelp =
    [ "quit                               - quit"
    , "help                               - show this help"
    ]

