module LibMain (handle) where

import BuiltIn qualified
import Control.Monad.Trans.State
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Evaluator qualified
import Formatter qualified
import Parser
import Typechecker qualified
import Prelude

handle :: String -> IO ()
handle inputFilePath = do
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
    Right e -> do
      case evalStateT (Typechecker.typecheckExpr1 id BuiltIn.initialTypeEnv e) () of
        Left (tyErr, _travMod) -> do
          putStrLn "-------- type error: --------"
          putStrLn $ Text.unpack $ Formatter.render tyErr
        Right (a1tye, a1e) -> do
          putStrLn "-------- type: --------"
          putStrLn $ Text.unpack $ Formatter.render a1tye
          putStrLn "-------- elaborated expression: --------"
          putStrLn $ Text.unpack $ Formatter.render a1e
          case evalStateT (Evaluator.evalExpr1 BuiltIn.initialEnv a1e) Evaluator.initialState of
            Left err -> do
              putStrLn "-------- eval error: --------"
              print err
            Right a1v -> do
              putStrLn "-------- generated code: --------"
              putStrLn $ Text.unpack $ Formatter.render a1v
              let a0e = Evaluator.unliftVal a1v
              case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0e) Evaluator.initialState of
                Left err -> do
                  putStrLn "-------- eval error: --------"
                  print err
                Right a0v -> do
                  putStrLn "-------- final result: --------"
                  putStrLn $ Text.unpack $ Formatter.render a0v
