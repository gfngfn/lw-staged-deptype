module Surface.SurfaceMain
  ( Argument (..),
    handle,
  )
where

import Data.Text.IO qualified as TextIO
import Surface.BindingTimeAnalyzer qualified as BindingTimeAnalyzer
import Surface.BuiltIn qualified as BuiltIn
import Surface.Parser qualified as Parser
import Prelude

newtype Argument = Argument
  { inputFilePath :: String
  }

handle :: Argument -> IO Bool
handle Argument {inputFilePath} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
      failure
    Right e -> do
      case BindingTimeAnalyzer.run BuiltIn.initialBindingTimeEnv e of
        Left err -> do
          putStrLn "-------- binding-time analysis error: --------"
          print err
          failure
        Right r -> do
          print r
          success
  where
    success = return True
    failure = return False
