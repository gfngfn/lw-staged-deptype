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
import Lwsd.SrcSyntax
import Lwsd.Syntax
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

typecheckAndEval :: Argument -> SourceSpec -> Expr -> IO Bool
typecheckAndEval Argument {optimize, distributeIf, displayWidth, compileTimeOnly} sourceSpec e = do
  let initialEvalState = Evaluator.initialState sourceSpec
  let typecheckerConfig =
        TypecheckState
          { optimizeTrivialAssertion = optimize,
            distributeIfUnderTensorShape = distributeIf,
            sourceSpec,
            nextVarIndex = 0
          }
  case evalStateT (Typechecker.typecheckExpr0 id BuiltIn.initialTypeEnv [] e) typecheckerConfig of
    Left (tyErr, _travMod) -> do
      putStrLn "-------- type error: --------"
      putRenderedLines tyErr
      failure
    Right (result, a0e) -> do
      putStrLn "-------- type: --------"
      putRenderedLinesAtStage0 result
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

-- Returns a boolean that represents success or failure
handle :: Argument -> IO Bool
handle arg@Argument {inputFilePath, stubFilePath, displayWidth} = do
  putStrLn "Lightweight Dependent Types via Staging"
  stub <- TextIO.readFile stubFilePath
  case Parser.parseDecls stub of
    Left err -> do
      putStrLn "-------- parse error of stub: --------"
      putStrLn err
      failure
    Right decls -> do
      putStrLn "-------- parsed stub: --------"
      print decls
      source <- TextIO.readFile inputFilePath
      case Parser.parseExpr source of
        Left err -> do
          putStrLn "-------- parse error of source: --------"
          putStrLn err
          failure
        Right e -> do
          putStrLn "-------- parsed expression: --------"
          putRenderedLinesAtStage0 e
          let sourceSpec =
                SourceSpec
                  { LocationInFile.source = source,
                    LocationInFile.inputFilePath = inputFilePath
                  }
          typecheckAndEval arg sourceSpec e
  where
    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth
