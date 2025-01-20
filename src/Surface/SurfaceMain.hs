module Surface.SurfaceMain
  ( Argument (..),
    handle,
  )
where

import Data.Map qualified as Map
import Data.Text.IO qualified as TextIO
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Lwsd.LibMain qualified as LwsdMain
import Lwsd.Parser qualified as LwsdParser
import Lwsd.TypeEnv (Entry (..), SigRecord)
import Surface.BindingTime qualified as BindingTime
import Surface.BindingTime.Core
import Surface.BuiltIn qualified as BuiltIn
import Surface.Parser qualified as Parser
import Util.LocationInFile (SourceSpec (SourceSpec))
import Util.LocationInFile qualified as LocationInFile
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    stubFilePath :: String,
    optimize :: Bool,
    distributeIf :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool,
    fallBackToBindingTime0 :: Bool
  }

makeBindingTimeEnvFromStub :: SigRecord -> BindingTimeEnv
makeBindingTimeEnvFromStub =
  Map.foldrWithKey
    ( \_var entry _bindingTimeEnv ->
        case entry of
          Ass0Entry _a0tye _ -> error "TODO: makeBindingTimeEnvFromStub, Ass0Entry"
          Ass1Entry _a1tye -> error "TODO: makeBindingTimeEnvFromStub, Ass1Entry"
          AssPersEntry _aPtye _ -> error "TODO: makeBindingTimeEnvFromStub, AssPersEntry"
    )
    Map.empty

handle :: Argument -> IO Bool
handle Argument {inputFilePath, stubFilePath, optimize, distributeIf, displayWidth, compileTimeOnly, fallBackToBindingTime0} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  let lwArg =
        LwsdMain.Argument
          { LwsdMain.inputFilePath = inputFilePath,
            LwsdMain.stubFilePath = stubFilePath,
            LwsdMain.optimize = optimize,
            LwsdMain.distributeIf = distributeIf,
            LwsdMain.displayWidth = displayWidth,
            LwsdMain.compileTimeOnly = compileTimeOnly
          }
  stub <- TextIO.readFile stubFilePath
  case LwsdParser.parseDecls stub of
    Left err -> do
      putStrLn "-------- parse error of stub: --------"
      putStrLn err
      failure
    Right declsInStub -> do
      let sourceSpecOfStub =
            SourceSpec
              { LocationInFile.source = stub,
                LocationInFile.inputFilePath = stubFilePath
              }
      case LwsdMain.typecheckStub lwArg sourceSpecOfStub declsInStub of
        Left tyErr -> do
          putStrLn "-------- type error of stub: --------"
          putRenderedLines tyErr
          failure
        Right (tyEnvStub, _sigr, stateAfterTraversingStub) -> do
          -- let initialBindingTimeEnv = makeBindingTimeEnvFromStub sigr -- TODO: use this
          let initialBindingTimeEnv = BuiltIn.initialBindingTimeEnv -- TODO: remove this
          source <- TextIO.readFile inputFilePath
          let sourceSpecOfInput =
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
              case BindingTime.analyze sourceSpecOfInput fallBackToBindingTime0 initialBindingTimeEnv e of
                Left analyErr -> do
                  putStrLn "-------- binding-time analysis error: --------"
                  putRenderedLines analyErr
                  failure
                Right (bce, lwe) -> do
                  putStrLn "-------- result of binding-time analysis: --------"
                  putRenderedLines bce
                  putStrLn "-------- result of staging: --------"
                  putRenderedLinesAtStage0 lwe
                  LwsdMain.typecheckAndEvalInput lwArg stateAfterTraversingStub sourceSpecOfInput tyEnvStub lwe
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    failure = return False
