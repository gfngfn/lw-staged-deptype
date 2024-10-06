module Main where

import Lwsd.LibMain qualified as LwsdMain
import Options.Applicative
import Surface.SurfaceMain qualified as SurfaceMain
import System.Exit

data Argument
  = LwsdArgument LwsdMain.Argument
  | SurfaceArgument SurfaceMain.Argument

argumentParser :: Parser Argument
argumentParser =
  subparser
    ( command "lwsd" (info (LwsdArgument <$> lwsdArgumentParser) (progDesc "Handles staged programs"))
        <> command "surface" (info (SurfaceArgument <$> surfaceArgumentParser) (progDesc "Handles non-staged programs"))
    )

lwsdArgumentParser :: Parser LwsdMain.Argument
lwsdArgumentParser =
  LwsdMain.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> switch (short 'o' <> long "optimize" <> help "Do slight optimization about assertion insertion")
    <*> option auto (value 80 <> short 'w' <> long "display-width" <> help "Set display width")
    <*> switch (long "compile-time-only" <> help "Execute only compile-time computation")

surfaceArgumentParser :: Parser SurfaceMain.Argument
surfaceArgumentParser =
  SurfaceMain.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")

main :: IO ()
main = do
  arg <- execParser (info argumentParser briefDesc)
  wasSuccess <-
    case arg of
      LwsdArgument lwsdArg -> LwsdMain.handle lwsdArg
      SurfaceArgument surfaceArg -> SurfaceMain.handle surfaceArg
  if wasSuccess
    then exitSuccess
    else exitFailure
