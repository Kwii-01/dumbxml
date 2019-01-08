module Main where

import Lib
import DumpTree
import System.Exit
import System.Environment

main :: IO ()
main = do
      args <- getArgs
      if (length args) /= 1 then
            exitWith $ ExitFailure 84
      else do
            contents <- readFile (args !! 0)
            dump (parser contents)
            exitWith ExitSuccess
