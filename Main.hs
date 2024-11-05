module Main where

import Clckwrks.CLI.Core
import Clckwrks.CLI.ProfileData
import Clckwrks.CLI.Rebac (rebacCLIHandler)
import System.FilePath ((</>))
import System.Environment (getArgs)

main :: IO ()
main =
  do args <- getArgs
     case args of
       (basePath:rest) ->
         do let mSchemaPath = case rest of
                             []    -> Nothing
                             [pth] -> Just pth
            u <- userCLIHandler basePath
            r <- rebacCLIHandler basePath mSchemaPath
            loop [u, r]
            putStrLn "type 'help' for a list of commands."
       _ -> putStrLn "Usage: clckwrks-cli path/to/_state [path to rebac schema]"
