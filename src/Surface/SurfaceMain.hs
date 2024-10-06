module Surface.SurfaceMain
  ( Argument (..),
    handle,
  )
where

import Control.Monad.Trans.State
import Data.Text.IO qualified as TextIO
import Surface.BindingTimeAnalyzer qualified as BindingTimeAnalyzer
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
      let be = evalState (BindingTimeAnalyzer.assignBindingTimeVarToExpr e) BindingTimeAnalyzer.initialState
      print be
      success
  where
    success = return True
    failure = return False
