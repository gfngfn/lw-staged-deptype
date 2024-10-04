module Main where

import Lwsd.LibMain
import Options.Applicative
import System.Exit

argumentParser :: Parser Argument
argumentParser =
  Argument
    <$> strArgument (metavar "STRING")
    <*> switch (short 'o' <> long "optimize" <> help "Do slight optimization about assertion insertion")
    <*> option auto (value 80 <> short 'w' <> long "display-width" <> help "Set display width")
    <*> switch (long "compile-time-only" <> help "Execute only compile-time computation")

main :: IO ()
main = do
  arg <- execParser (info argumentParser briefDesc)
  wasSuccess <- handle arg
  if wasSuccess
    then exitSuccess
    else exitFailure
