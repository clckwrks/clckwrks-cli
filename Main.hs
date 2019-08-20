module Main where

import Clckwrks.CLI.Core
import Clckwrks.CLI.ProfileData
import System.FilePath ((</>))
import System.Environment (getArgs)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [basePath] ->
         do u <- userCLIHandler basePath
            loop [u]
            putStrLn "type 'help' for a list of commands."
       _ -> putStrLn "Usage: clckwrks-cli path/to/_state"
