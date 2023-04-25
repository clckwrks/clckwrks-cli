{-# language CPP #-}
module Clckwrks.CLI.ProfileData where

import Control.Applicative ((<$>), (<*>), (*>), pure)
import Clckwrks (UserId(..))
import Clckwrks.CLI.Core (CLIHandler(..))
import Clckwrks.ProfileData.Acid (ProfileDataState(..), GetProfileData(..), GetUserIdDisplayNames(..), AddRole(..), RemoveRole(..))
import Clckwrks.ProfileData.Types (Role(..))
import Control.Monad.Reader
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')

#if MIN_VERSION_network(3,0,0)
import Network.Socket (SockAddr(..))
import Data.Acid.Remote (openRemoteStateSockAddr, skipAuthenticationPerform)
#else
import Network (PortID(UnixSocket))
import Data.Acid.Remote (openRemoteState, skipAuthenticationPerform)
#endif
import System.Environment
import System.FilePath ((</>))
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
{-
main :: IO ()
main =
    do args <- getArgs
       case args of
         [socket] ->
             do acid <- openRemoteState skipAuthenticationPerform "localhost" (UnixSocket socket)
                putStrLn "type 'help' for a list of commands."
                runReaderT (runInputT defaultSettings loop) acid
         _ -> putStrLn "Usage: clckwrks-cli path/to/profileData_socket"
-}

data UserCmd
    = UCList
    | UCShow UserId
    | UCAddRole UserId Role
    | UCRemoveRole UserId Role
      deriving (Eq, Ord, Read, Show)

showUserHelp :: [String]
showUserHelp =
    [ "user list                          - show all users"
    , "user show <userid>                 - show profile data for <userid>"
    , "user add-role <userid> <role>      - add a role (such as Administrator)"
    , "user remove-role <userid> <role>   - remove a role"
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

execUserCommand :: UserCmd -> ReaderT (AcidState ProfileDataState) IO ()
execUserCommand UCList =
    do a <- ask
       all <- query' a GetUserIdDisplayNames
       lift $ print all
       return ()
execUserCommand (UCShow uid) =
    do a <- ask
       pd <- query' a (GetProfileData uid)
       lift $ print pd
execUserCommand (UCAddRole uid role) =
    do a <- ask
       update' a (AddRole uid role)
execUserCommand (UCRemoveRole uid role) =
    do a <- ask
       update' a (RemoveRole uid role)

initUserCommand :: FilePath -> IO (UserCmd -> IO ())
initUserCommand basePath =
#if MIN_VERSION_network(3,0,0)
  do profileData <- openRemoteStateSockAddr skipAuthenticationPerform (SockAddrUnix ((basePath </> "profileData_socket")))
#else
  do profileData <- openRemoteState skipAuthenticationPerform "localhost" (UnixSocket ((basePath </> "profileData_socket")))
#endif
     pure $ \c -> runReaderT (execUserCommand c) profileData

userCLIHandler :: FilePath -> IO CLIHandler
userCLIHandler basePath =
  do exec <- initUserCommand basePath
     pure $ CLIHandler
       { cliPrefix = "user"
       , cliExec   = exec
       , cliParser = pUserCmd
       , cliHelp   = showUserHelp
       }
