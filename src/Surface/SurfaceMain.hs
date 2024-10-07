module Surface.SurfaceMain
  ( Argument (..),
    handle,
  )
where

import Data.Text.IO qualified as TextIO
import Lwsd.Evaluator qualified as Evaluator
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Lwsd.LibMain qualified as LwsdMain
import Surface.BindingTimeAnalyzer qualified as BindingTimeAnalyzer
import Surface.BuiltIn qualified as BuiltIn
import Surface.Parser qualified as Parser
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    optimize :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool
  }

handle :: Argument -> IO Bool
handle Argument {inputFilePath, optimize, displayWidth, compileTimeOnly} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
      failure
    Right e -> do
      putStrLn "-------- parsed expression: --------"
      putRenderedLines e
      case BindingTimeAnalyzer.run BuiltIn.initialBindingTimeEnv e of
        Left analyErr -> do
          putStrLn "-------- binding-time analysis error: --------"
          putRenderedLines analyErr
          failure
        Right (bce, lwe) -> do
          putStrLn "-------- result of binding-time analysis: --------"
          putRenderedLines bce
          putStrLn "-------- result of staging: --------"
          putRenderedLinesAtStage0 lwe
          let lwArg =
                LwsdMain.Argument
                  { LwsdMain.inputFilePath = inputFilePath,
                    LwsdMain.optimize = optimize,
                    LwsdMain.displayWidth = displayWidth,
                    LwsdMain.compileTimeOnly = compileTimeOnly
                  }
          let sourceSpec = Evaluator.SourceSpec source inputFilePath
          LwsdMain.typecheckAndEval lwArg sourceSpec lwe
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    failure = return False
