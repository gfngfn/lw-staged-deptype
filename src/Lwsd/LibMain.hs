module Lwsd.LibMain
  ( Argument (..),
    typecheckStub,
    typecheckAndEvalInput,
    handle,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Either.Extra (mapLeft)
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
import Lwsd.Typechecker (TypecheckConfig (..), TypecheckState (..))
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

type M a = ReaderT Argument IO a

makeConfig :: SourceSpec -> M TypecheckConfig
makeConfig sourceSpec = do
  Argument {optimize, distributeIf} <- ask
  pure $
    TypecheckConfig
      { optimizeTrivialAssertion = optimize,
        distributeIfUnderTensorShape = distributeIf,
        sourceSpec = sourceSpec
      }

success, failure :: M Bool
success = pure True
failure = pure False

putSectionLine :: String -> M ()
putSectionLine s =
  lift $ putStrLn ("-------- " ++ s ++ " --------")

putRenderedLines :: (Disp a) => a -> M ()
putRenderedLines v = do
  Argument {displayWidth} <- ask
  lift $ Formatter.putRenderedLines displayWidth v

putRenderedLinesAtStage0 :: (Disp a) => a -> M ()
putRenderedLinesAtStage0 v = do
  Argument {displayWidth} <- ask
  lift $ Formatter.putRenderedLinesAtStage0 displayWidth v

putRenderedLinesAtStage1 :: (Disp a) => a -> M ()
putRenderedLinesAtStage1 v = do
  Argument {displayWidth} <- ask
  lift $ Formatter.putRenderedLinesAtStage1 displayWidth v

typecheckStub :: SourceSpec -> [Bind] -> M (Either TypeError ((TypeEnv, SigRecord, [AssBind]), TypecheckState))
typecheckStub sourceSpecOfStub bindsInStub = do
  tcConfig <- makeConfig sourceSpecOfStub
  let tcState = TypecheckState {nextVarIndex = 0}
      initialTypeEnv = TypeEnv.empty
  pure $
    mapLeft fst $
      Typechecker.run (Typechecker.typecheckBinds () initialTypeEnv bindsInStub) tcConfig tcState

typecheckInput :: SourceSpec -> TypecheckState -> TypeEnv -> Expr -> M (Either TypeError (Result Ass0TypeExpr, Ass0Expr))
typecheckInput sourceSpecOfInput tcState tyEnvStub e = do
  tcConfig <- makeConfig sourceSpecOfInput
  pure $
    mapLeft fst $
      fst <$> Typechecker.run (Typechecker.typecheckExpr0 () tyEnvStub [] e) tcConfig tcState

typecheckAndEvalInput :: TypecheckState -> SourceSpec -> TypeEnv -> [AssBind] -> Expr -> M Bool
typecheckAndEvalInput tcState sourceSpecOfInput tyEnvStub abinds e = do
  let initialEvalState = Evaluator.initialState sourceSpecOfInput
  r <- typecheckInput sourceSpecOfInput tcState tyEnvStub e
  case r of
    Left tyErr -> do
      putSectionLine "type error:"
      putRenderedLines tyErr
      failure
    Right (result, a0eWithoutStub) -> do
      let a0e = makeExprFromBinds abinds a0eWithoutStub
      putSectionLine "type:"
      putRenderedLinesAtStage0 result
      putSectionLine "elaborated expression:"
      putRenderedLinesAtStage0 a0e
      case evalStateT (Evaluator.evalExpr0 initialEnv a0e) initialEvalState of
        Left err -> do
          putSectionLine "error during compile-time code generation:"
          putRenderedLines err
          failure
        Right a0v -> do
          Argument {compileTimeOnly} <- ask
          case a0v of
            A0ValBracket a1v -> do
              putSectionLine "generated code:"
              putRenderedLinesAtStage1 a1v
              let a0eRuntime = Evaluator.unliftVal a1v
              if compileTimeOnly
                then success
                else case evalStateT (Evaluator.evalExpr0 initialEnv a0eRuntime) initialEvalState of
                  Left err -> do
                    putSectionLine "eval error:"
                    putRenderedLines err
                    failure
                  Right a0vRuntime -> do
                    putSectionLine "result of runtime evaluation:"
                    putRenderedLinesAtStage0 a0vRuntime
                    success
            _ -> do
              putSectionLine "stage-0 result:"
              lift $ putStrLn "(The stage-0 result was not a code value)"
              putRenderedLinesAtStage0 a0v
              if compileTimeOnly
                then success
                else failure
  where
    initialEnv :: EvalEnv
    initialEnv = Map.empty

typecheckAndEval :: SourceSpec -> [Bind] -> SourceSpec -> Expr -> M Bool
typecheckAndEval sourceSpecOfStub bindsInStub sourceSpecOfInput e = do
  r <- typecheckStub sourceSpecOfStub bindsInStub
  case r of
    Left tyErr -> do
      putSectionLine "type error by stub"
      putRenderedLines tyErr
      failure
    Right ((tyEnvStub, _sigr, abinds), tcState) -> do
      typecheckAndEvalInput tcState sourceSpecOfInput tyEnvStub abinds e

handle' :: M Bool
handle' = do
  Argument {inputFilePath, stubFilePath} <- ask
  lift $ putStrLn "Lightweight Dependent Types via Staging"
  stub <- lift $ TextIO.readFile stubFilePath
  case Parser.parseBinds stub of
    Left err -> do
      putSectionLine "parse error of stub:"
      lift $ putStrLn err
      failure
    Right bindsInStub -> do
      source <- lift $ TextIO.readFile inputFilePath
      case Parser.parseExpr source of
        Left err -> do
          putSectionLine "parse error of source:"
          lift $ putStrLn err
          failure
        Right e -> do
          putSectionLine "parsed expression:"
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
          typecheckAndEval sourceSpecOfStub bindsInStub sourceSpecOfInput e

-- Returns a boolean that represents success or failure
handle :: Argument -> IO Bool
handle = runReaderT handle'
