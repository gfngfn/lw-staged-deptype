module Main where

import LibMain qualified
import System.Environment qualified as Env

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [inputFilePath] -> do
      putStrLn "Hello, Haskell!"
      LibMain.handle inputFilePath
    _ ->
      error "Wrong number of arguments"
