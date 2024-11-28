module Main where

import Lwsd.LibMain qualified as LwsdMain
import Options.Applicative
import Surface.SurfaceMain qualified as SurfaceMain
import System.Exit

defaultDisplayWidth :: Int
defaultDisplayWidth = 120

helpDisplayWidth, helpOptimize, helpDistributeIf, helpCompileTimeOnly :: String
helpDisplayWidth = "Set the display width (default: " ++ show defaultDisplayWidth ++ ")"
helpOptimize = "Inserts only non-trivial cast assertions"
helpDistributeIf = "Distributes if-expressions under list literals for tensor shapes"
helpCompileTimeOnly = "Stops after the compile-time evaluation"

data Argument
  = LwsdArgument LwsdMain.Argument
  | SurfaceArgument SurfaceMain.Argument

argumentParser :: Parser Argument
argumentParser =
  subparser
    ( command "lwsd" (info (LwsdArgument <$> lwsdArgumentParser <**> helper) (progDesc "Handles staged programs"))
        <> command "surface" (info (SurfaceArgument <$> surfaceArgumentParser <**> helper) (progDesc "Handles non-staged programs"))
    )

lwsdArgumentParser :: Parser LwsdMain.Argument
lwsdArgumentParser =
  LwsdMain.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> switch (short 'O' <> long "optimize" <> help helpOptimize)
    <*> switch (short 'D' <> long "distribute-if" <> help helpDistributeIf)
    <*> option auto (short 'w' <> long "display-width" <> value defaultDisplayWidth <> help helpDisplayWidth)
    <*> switch (short 'c' <> long "compile-time-only" <> help helpCompileTimeOnly)

surfaceArgumentParser :: Parser SurfaceMain.Argument
surfaceArgumentParser =
  SurfaceMain.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> switch (short 'O' <> long "optimize" <> help helpOptimize)
    <*> switch (short 'D' <> long "distribute-if" <> help helpDistributeIf)
    <*> option auto (short 'w' <> long "display-width" <> value defaultDisplayWidth <> help helpDisplayWidth)
    <*> switch (short 'c' <> long "compile-time-only" <> help helpCompileTimeOnly)
    <*> switch (short 'd' <> long "default-to-stage-0" <> help "Make ambiguous binding times default to 0, which promotes inlining")

main :: IO ()
main = do
  arg <- execParser (info (argumentParser <**> helper) briefDesc)
  wasSuccess <-
    case arg of
      LwsdArgument lwsdArg -> LwsdMain.handle lwsdArg
      SurfaceArgument surfaceArg -> SurfaceMain.handle surfaceArg
  if wasSuccess
    then exitSuccess
    else exitFailure
