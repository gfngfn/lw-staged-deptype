module Lwsd.LibMain
  ( Argument (..),
    typecheckAndEval,
    handle,
  )
where

import Control.Monad.Trans.State
import Data.Text.IO qualified as TextIO
import Lwsd.BuiltIn qualified as BuiltIn
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

success, failure :: IO Bool
success = return True
failure = return False

typecheckAndEval :: Argument -> Evaluator.SourceSpec -> Expr -> IO Bool
typecheckAndEval Argument {optimize, displayWidth, compileTimeOnly} sourceSpec e = do
  let initialEvalState = Evaluator.initialState sourceSpec
  case evalStateT (Typechecker.typecheckExpr0 id BuiltIn.initialTypeEnv e) typecheckerConfig of
    Left (tyErr, _travMod) -> do
      putStrLn "-------- type error: --------"
      putRenderedLines tyErr
      failure
    Right (a0tye, a0e) -> do
      putStrLn "-------- type: --------"
      putRenderedLinesAtStage0 a0tye
      putStrLn "-------- elaborated expression: --------"
      putRenderedLinesAtStage0 a0e
      case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0e) initialEvalState of
        Left err -> do
          putStrLn "-------- error during compile-time code generation: --------"
          putRenderedLines err
          failure
        Right a0v -> do
          case a0v of
            A0ValBracket a1v -> do
              putStrLn "-------- generated code: --------"
              putRenderedLinesAtStage1 a1v
              let a0eRuntime = Evaluator.unliftVal a1v
              if compileTimeOnly
                then success
                else case evalStateT (Evaluator.evalExpr0 BuiltIn.initialEnv a0eRuntime) initialEvalState of
                  Left err -> do
                    putStrLn "-------- eval error: --------"
                    putRenderedLines err
                    failure
                  Right a0vRuntime -> do
                    putStrLn "-------- result of runtime evaluation: --------"
                    putRenderedLinesAtStage0 a0vRuntime
                    success
            _ -> do
              putStrLn "-------- stage-0 result: --------"
              putStrLn "(The stage-0 result was not a code value)"
              putRenderedLinesAtStage0 a0v
              if compileTimeOnly
                then success
                else failure
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    putRenderedLinesAtStage1 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage1 = Formatter.putRenderedLinesAtStage1 displayWidth

    typecheckerConfig :: TypecheckState
    typecheckerConfig = TypecheckState {optimizeTrivialAssertion = optimize, nextVarIndex = 0}

-- Returns a boolean that represents success or failure
handle :: Argument -> IO Bool
handle arg@Argument {inputFilePath} = do
  putStrLn "Lightweight Dependent Types via Staging"
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
      failure
    Right e ->
      let sourceSpec = Evaluator.SourceSpec source inputFilePath
       in typecheckAndEval arg sourceSpec e
