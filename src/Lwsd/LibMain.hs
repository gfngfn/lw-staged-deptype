module Lwsd.LibMain
  ( Argument (..),
    typecheckStub,
    typecheckAndEvalInput,
    handle,
  )
where

import Control.Monad.Trans.State
import Data.Map qualified as Map
import Data.Text.IO qualified as TextIO
import Lwsd.Evaluator qualified as Evaluator
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Lwsd.Parser qualified as Parser
import Lwsd.Scope.SigRecord (SigRecord)
import Lwsd.Scope.TypeEnv (TypeEnv)
import Lwsd.Scope.TypeEnv qualified as TypeEnv
import Lwsd.SrcSyntax
import Lwsd.Syntax
import Lwsd.TypeError (TypeError)
import Lwsd.Typechecker (TypecheckState (..))
import Lwsd.Typechecker qualified as Typechecker
import Util.LocationInFile (SourceSpec (SourceSpec))
import Util.LocationInFile qualified as LocationInFile
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    stubFilePath :: String,
    optimize :: Bool,
    distributeIf :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool
  }
  deriving (Read, Show)

success, failure :: IO Bool
success = return True
failure = return False

typecheckStub :: Argument -> SourceSpec -> [Bind] -> Either TypeError (TypeEnv, SigRecord, TypecheckState)
typecheckStub Argument {optimize, distributeIf} sourceSpecOfStub bindsInStub = do
  let typecheckerConfigOfStub =
        TypecheckState
          { optimizeTrivialAssertion = optimize,
            distributeIfUnderTensorShape = distributeIf,
            sourceSpec = sourceSpecOfStub,
            nextVarIndex = 0
          }
      initialTypeEnv = TypeEnv.empty
  case runStateT (Typechecker.typecheckBinds id initialTypeEnv bindsInStub) typecheckerConfigOfStub of
    Left (tyErr, _travMod) ->
      Left tyErr
    Right ((tyEnvStub, sigr), stateAfterTraversingStub) -> do
      pure (tyEnvStub, sigr, stateAfterTraversingStub)

typecheckInput :: TypecheckState -> TypeEnv -> Expr -> Either TypeError (Result Ass0TypeExpr, Ass0Expr)
typecheckInput typecheckerConfigOfInput tyEnvStub e =
  case evalStateT (Typechecker.typecheckExpr0 id tyEnvStub [] e) typecheckerConfigOfInput of
    Left (tyErr, _travMod) -> Left tyErr
    Right resultAndExpr -> pure resultAndExpr

typecheckAndEvalInput :: Argument -> TypecheckState -> SourceSpec -> TypeEnv -> Expr -> IO Bool
typecheckAndEvalInput Argument {compileTimeOnly, displayWidth} stateAfterTraversingStub sourceSpecOfInput tyEnvStub e = do
  let initialEvalState = Evaluator.initialState sourceSpecOfInput
  let typecheckerConfigOfInput = stateAfterTraversingStub {sourceSpec = sourceSpecOfInput}
  case typecheckInput typecheckerConfigOfInput tyEnvStub e of
    Left tyErr -> do
      putStrLn "-------- type error: --------"
      putRenderedLines tyErr
      failure
    Right (result, a0e) -> do
      putStrLn "-------- type: --------"
      putRenderedLinesAtStage0 result
      putStrLn "-------- elaborated expression: --------"
      putRenderedLinesAtStage0 a0e
      case evalStateT (Evaluator.evalExpr0 initialEnv a0e) initialEvalState of
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
                else case evalStateT (Evaluator.evalExpr0 initialEnv a0eRuntime) initialEvalState of
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
    initialEnv :: EvalEnv
    initialEnv = Map.empty

    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    putRenderedLinesAtStage1 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage1 = Formatter.putRenderedLinesAtStage1 displayWidth

typecheckAndEval :: Argument -> SourceSpec -> [Bind] -> SourceSpec -> Expr -> IO Bool
typecheckAndEval arg@Argument {displayWidth} sourceSpecOfStub bindsInStub sourceSpecOfInput e = do
  case typecheckStub arg sourceSpecOfStub bindsInStub of
    Left tyErr -> do
      putStrLn "-------- type error by stub: --------"
      putRenderedLines tyErr
      failure
    Right (tyEnvStub, _sigr, stateAfterTraversingStub) -> do
      typecheckAndEvalInput arg stateAfterTraversingStub sourceSpecOfInput tyEnvStub e
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

-- Returns a boolean that represents success or failure
handle :: Argument -> IO Bool
handle arg@Argument {inputFilePath, stubFilePath, displayWidth} = do
  putStrLn "Lightweight Dependent Types via Staging"
  stub <- TextIO.readFile stubFilePath
  case Parser.parseBinds stub of
    Left err -> do
      putStrLn "-------- parse error of stub: --------"
      putStrLn err
      failure
    Right bindsInStub -> do
      source <- TextIO.readFile inputFilePath
      case Parser.parseExpr source of
        Left err -> do
          putStrLn "-------- parse error of source: --------"
          putStrLn err
          failure
        Right e -> do
          putStrLn "-------- parsed expression: --------"
          putRenderedLinesAtStage0 e
          let sourceSpecOfInput =
                SourceSpec
                  { LocationInFile.source = source,
                    LocationInFile.inputFilePath = inputFilePath
                  }
              sourceSpecOfStub =
                SourceSpec
                  { LocationInFile.source = stub,
                    LocationInFile.inputFilePath = stubFilePath
                  }
          typecheckAndEval arg sourceSpecOfStub bindsInStub sourceSpecOfInput e
  where
    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth
