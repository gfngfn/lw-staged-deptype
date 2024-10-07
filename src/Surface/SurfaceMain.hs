module Surface.SurfaceMain
  ( Argument (..),
    handle,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Surface.BindingTimeAnalyzer qualified as BindingTimeAnalyzer
import Surface.BuiltIn qualified as BuiltIn
import Surface.Parser qualified as Parser
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    displayWidth :: Int
  }

handle :: Argument -> IO Bool
handle Argument {inputFilePath, displayWidth} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
      failure
    Right e -> do
      case BindingTimeAnalyzer.run BuiltIn.initialBindingTimeEnv e of
        Left analyErr -> do
          putStrLn "-------- binding-time analysis error: --------"
          putRenderedLines analyErr
          failure
        Right (bce, lwe) -> do
          putStrLn "-------- result of binding-time analysis (B): --------"
          putRenderedLines bce
          putStrLn "-------- result of binding-time analysis: --------"
          putRenderedLines lwe
          success
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines x =
      putStrLn $ Text.unpack $ Formatter.render displayWidth x

    success = return True
    failure = return False
