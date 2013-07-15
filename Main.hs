module Main where

import Control.Applicative ((<$>), (<*>), (*>), pure)
import Clckwrks (UserId(..))
import Clckwrks.ProfileData.Acid (ProfileDataState(..), GetProfileData(..), AddRole(..), RemoveRole(..), GetUserIdUsernames(..))
import Clckwrks.ProfileData.Types (Role(..))
import Control.Monad.Reader
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Remote (openRemoteState, skipAuthenticationPerform)
import Network (PortID(UnixSocket))
import System.Environment
import System.Console.Haskeline
import Text.Parsec
import Text.Parsec.String

-- right now this just connects to the server and makes UserId 1 an administrator
--
-- eventually there should be an actually useful command-line interface
{-
main :: IO ()
main =
    do [socket] <- getArgs
       acid <- openRemoteState "localhost" (UnixSocket socket)
       update acid (AddRole (UserId 1) Administrator)
       pd <- query acid (GetProfileData (UserId 1))
       print pd
-}
main :: IO ()
main =
    do args <- getArgs
       case args of
         [socket] ->
             do acid <- openRemoteState skipAuthenticationPerform "localhost" (UnixSocket socket)
                putStrLn "type 'help' for a list of commands."
                runReaderT (runInputT defaultSettings loop) acid
         _ -> putStrLn "Usage: clckwrks-cli path/to/profileData_socket"

data UserCmd
    = UCList
    | UCShow UserId
    | UCAddRole UserId Role
    | UCRemoveRole UserId Role
      deriving (Eq, Ord, Read, Show)

data Command
    = User UserCmd
    | Quit
    | Help
      deriving (Eq, Ord, Read, Show)

showHelp :: String
showHelp = unlines
    [ "user list                          - show all users"
    , "user show <userid>                 - show profile data for <userid>"
    , "user add-role <userid> <role>      - add a role (such as Administrator)"
    , "user remove-role <userid> <role>   - remove a role"
    , "quit                               - quit"
    , "help                               - show this help"
    ]

pRole :: Parser Role
pRole =
    string "Administrator" *> pure Administrator

pUserId :: Parser UserId
pUserId = UserId <$> (read <$> many1 digit)

pUserCmd :: Parser UserCmd
pUserCmd =
       do string "list"
          return UCList
       <|>
       do string "show"
          skipMany1 space
          u <- pUserId
          return (UCShow u)
       <|>
       do string "add-role"
          skipMany1 space
          u <- pUserId
          skipMany1 space
          r <- pRole
          return (UCAddRole u r)
       <|>
       do string "remove-role"
          skipMany1 space
          u <- pUserId
          skipMany1 space
          r <- pRole
          return (UCRemoveRole u r)


pCommand :: Parser Command
pCommand =
    do string "user"
       skipMany1 space
       User <$> pUserCmd
    <|>
    do string "quit" *> return Quit
    <|>
    do string "help" *> return Help

loop :: InputT (ReaderT (AcidState ProfileDataState) IO) ()
loop = do
  minput <- getInputLine "% "
  case minput of
    Nothing     -> return ()
    Just input  ->
        do let r = parse pCommand input input
           case r of
             (Left e) ->
                 do liftIO $ print e
                    loop
             (Right Quit) ->
                    return ()
             (Right cmd) ->
                 do lift $ execCommand cmd
                    loop

execCommand :: Command -> ReaderT (AcidState ProfileDataState) IO ()
execCommand (User UCList) =
    do a <- ask
       all <- query' a GetUserIdUsernames
       lift $ print all
       return ()
execCommand (User (UCShow uid)) =
    do a <- ask
       mpd <- query' a (GetProfileData uid)
       case mpd of
         Nothing   -> lift $ putStrLn $ "Invalid userid."
         (Just pd) -> lift $ print pd
execCommand (User (UCAddRole uid role)) =
    do a <- ask
       update' a (AddRole uid role)
execCommand (User (UCRemoveRole uid role)) =
    do a <- ask
       update' a (RemoveRole uid role)
execCommand Help =
    do lift $ putStrLn $ showHelp
       return ()

