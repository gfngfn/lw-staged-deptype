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
import Lwsd.Scope.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Lwsd.Scope.SigRecord qualified as SigRecord
import Surface.BindingTime qualified as BindingTime
import Surface.BindingTime.Core
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
  SigRecord.fold
    ( \var entry bindingTimeEnv ->
        case entry of
          Ass0Entry a0tye a0metadataOpt ->
            case a0metadataOpt of
              Just Ass0Metadata {ass0surfaceName = x} ->
                Map.insert
                  x
                  (EntryBuiltInFixed var BT0 (fromStaged0 a0tye))
                  bindingTimeEnv
              Nothing ->
                bindingTimeEnv
          Ass1Entry a1tye a1metadataOpt ->
            case a1metadataOpt of
              Just Ass1Metadata {ass1surfaceName = x} ->
                Map.insert
                  x
                  (EntryBuiltInFixed var BT1 (fromStaged1 a1tye))
                  bindingTimeEnv
              Nothing ->
                bindingTimeEnv
          AssPersEntry aPtye AssPersMetadata {assPsurfaceName = x} ->
            Map.insert
              x
              (EntryBuiltInPersistent var (fromStagedPers aPtye))
              bindingTimeEnv
    )
    ( \varM (ModuleEntry sigr) bindingTimeEnv ->
        -- Reuses the module name `varM` in the core language for the surface language.
        Map.insert
          varM
          (EntryModule (makeBindingTimeEnvFromStub sigr))
          bindingTimeEnv
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
  case LwsdParser.parseBinds stub of
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
        Right (tyEnvStub, sigr, abinds, stateAfterTraversingStub) -> do
          let initialBindingTimeEnv = makeBindingTimeEnvFromStub sigr
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
                  LwsdMain.typecheckAndEvalInput lwArg stateAfterTraversingStub sourceSpecOfInput tyEnvStub abinds lwe
  where
    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    failure = return False
