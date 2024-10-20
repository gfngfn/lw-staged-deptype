module Surface.SurfaceMain
  ( Argument (..),
    handle,
  )
where

import Data.Text.IO qualified as TextIO
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Lwsd.LibMain qualified as LwsdMain
import Surface.BindingTime qualified as BindingTime
import Surface.BuiltIn qualified as BuiltIn
import Surface.Parser qualified as Parser
import Util.LocationInFile (SourceSpec (SourceSpec))
import Util.LocationInFile qualified as LocationInFile
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    optimize :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool,
    fallBackToBindingTime0 :: Bool
  }

handle :: Argument -> IO Bool
handle Argument {inputFilePath, optimize, displayWidth, compileTimeOnly, fallBackToBindingTime0} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  source <- TextIO.readFile inputFilePath
  let sourceSpec =
        SourceSpec
          { LocationInFile.source = source,
            LocationInFile.inputFilePath = inputFilePath
          }
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
      failure
    Right e -> do
      putStrLn "-------- parsed expression: --------"
      putRenderedLines e
      case BindingTime.analyze sourceSpec fallBackToBindingTime0 BuiltIn.initialBindingTimeEnv e of
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
          LwsdMain.typecheckAndEval lwArg sourceSpec lwe
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    failure = return False
