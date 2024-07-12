module Lwsd.LibMain
  ( Argument (..),
    handle,
  )
where

import Control.Monad.Trans.State
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Lwsd.BuiltIn qualified as BuiltIn
import Lwsd.Evaluator (SourceSpec (SourceSpec))
import Lwsd.Evaluator qualified as Evaluator
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Lwsd.Parser qualified as Parser
import Lwsd.Syntax
import Lwsd.Typechecker (TypecheckState (..))
import Lwsd.Typechecker qualified as Typechecker
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    optimize :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool
  }
  deriving (Read, Show)

-- Returns a boolean that represents success or failure
handle :: Argument -> IO Bool
handle Argument {inputFilePath, optimize, displayWidth, compileTimeOnly} = do
  putStrLn "Lightweight Dependent Types via Staging"
  source <- TextIO.readFile inputFilePath
  let initialEvalState = Evaluator.initialState (SourceSpec source inputFilePath)
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
      failure
    Right e -> do
      case evalStateT (Typechecker.typecheckExpr0 id BuiltIn.initialTypeEnv e) typecheckerConfig of
        Left (tyErr, _travMod) -> do
          putStrLn "-------- type error: --------"
          putRenderedLines tyErr
          failure
        Right (a1tye, a0e) -> do
          putStrLn "-------- type: --------"
          putRenderedLines a1tye
          putStrLn "-------- elaborated expression: --------"
          putRenderedLines a0e
          case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0e) initialEvalState of
            Left err -> do
              putStrLn "-------- error during compile-time code generation: --------"
              putRenderedLines err
              failure
            Right a0v -> do
              case a0v of
                A0ValBracket a1v -> do
                  putStrLn "-------- generated code: --------"
                  putRenderedLines a1v
                  let a0eRuntime = Evaluator.unliftVal a1v
                  if compileTimeOnly then
                    success
                  else
                    case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0eRuntime) initialEvalState of
                      Left err -> do
                        putStrLn "-------- eval error: --------"
                        putRenderedLines err
                        failure
                      Right a0vRuntime -> do
                        putStrLn "-------- result of runtime evaluation: --------"
                        putRenderedLines a0vRuntime
                        success
                _ -> do
                  putStrLn "-------- stage-0 result: --------"
                  putStrLn "(The stage-0 result was not a code value)"
                  putRenderedLines a0v
                  if compileTimeOnly then
                    success
                  else
                    failure
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines x =
      putStrLn $ Text.unpack $ Formatter.render displayWidth x

    typecheckerConfig :: TypecheckState
    typecheckerConfig = TypecheckState {optimizeTrivialAssertion = optimize, nextVarIndex = 0}

    success = return True
    failure = return False
