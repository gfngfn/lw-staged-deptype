module Main where

import Lwsd.LibMain
import Options.Applicative

argumentParser :: Parser Argument
argumentParser =
  Argument
    <$> strArgument (metavar "STRING")
    <*> switch (short 'o' <> long "optimize" <> help "Do slight optimization about assertion insertion")
    <*> option auto (value 80 <> short 'w' <> long "diplay-width" <> help "Set display width")

main :: IO ()
main = execParser (info argumentParser briefDesc) >>= handle
