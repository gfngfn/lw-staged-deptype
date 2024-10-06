module Surface.SurfaceMain where

import Data.Text.IO qualified as TextIO
import Prelude

newtype Argument = Argument
  { inputFilePath :: String
  }

handle :: Argument -> IO Bool
handle Argument {inputFilePath} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  _source <- TextIO.readFile inputFilePath
  return True -- TODO: implement parsing and binding-time analysis
