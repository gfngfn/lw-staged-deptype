module LibMain (handle) where

import BuiltIn qualified
import Control.Monad.Trans.State
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Evaluator qualified
import Formatter (Disp)
import Formatter qualified
import Parser
import Syntax
import Typechecker qualified
import Prelude

-- TODO: make this changeable by command lines
lineWidth :: Int
lineWidth = 120

putRenderedLines :: (Disp a) => a -> IO ()
putRenderedLines x =
  putStrLn $ Text.unpack $ Formatter.render lineWidth x

handle :: String -> IO ()
handle inputFilePath = do
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
    Right e -> do
      case evalStateT (Typechecker.typecheckExpr0 id BuiltIn.initialTypeEnv e) () of
        Left (tyErr, _travMod) -> do
          putStrLn "-------- type error: --------"
          putRenderedLines tyErr
        Right (a1tye, a0e) -> do
          putStrLn "-------- type: --------"
          putRenderedLines a1tye
          putStrLn "-------- elaborated expression: --------"
          putRenderedLines a0e
          case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0e) Evaluator.initialState of
            Left err -> do
              putStrLn "-------- error during compile-time code generation: --------"
              putRenderedLines err
            Right a0v -> do
              case a0v of
                A0ValBracket a1v -> do
                  putStrLn "-------- generated code: --------"
                  putRenderedLines a1v
                  let a0eRuntime = Evaluator.unliftVal a1v
                  case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0eRuntime) Evaluator.initialState of
                    Left err -> do
                      putStrLn "-------- eval error: --------"
                      putRenderedLines err
                    Right a0vRuntime -> do
                      putStrLn "-------- result of runtime evaluation: --------"
                      putRenderedLines a0vRuntime
                _ -> do
                  putStrLn "-------- stage-0 result: --------"
                  putStrLn "(The stage-0 result was not a code value)"
                  putRenderedLines a0v
