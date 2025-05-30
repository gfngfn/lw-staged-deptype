module Surface.Entrypoint
  ( Argument (..),
    handle,
  )
where

import Control.Monad.Trans.Reader
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as TextIO
import Lwsd.Entrypoint qualified
import Lwsd.Formatter (Disp)
import Lwsd.Formatter qualified as Formatter
import Lwsd.Parser qualified as LwsdParser
import Lwsd.Scope.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Lwsd.Scope.SigRecord qualified as SigRecord
import Lwsd.Typechecker (TypecheckState (..))
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
    ( \varVal entry bindingTimeEnv ->
        case entry of
          Ass0Entry a0tye a0metadataOpt ->
            let x =
                  -- Uses the same name if not specified
                  case a0metadataOpt of
                    Left Ass0Metadata {ass0surfaceName} -> fromMaybe varVal ass0surfaceName
                    Right _ -> varVal
             in Map.insert
                  x
                  (EntryBuiltInFixed varVal BT0 (fromStaged0 a0tye))
                  bindingTimeEnv
          Ass1Entry a1tye a1metadataOpt ->
            let x =
                  -- Uses the same name if not specified
                  case a1metadataOpt of
                    Left Ass1Metadata {ass1surfaceName} -> fromMaybe varVal ass1surfaceName
                    Right _ -> varVal
             in Map.insert
                  x
                  (EntryBuiltInFixed varVal BT1 (fromStaged1 a1tye))
                  bindingTimeEnv
          AssPersEntry aPtye AssPersMetadata {assPsurfaceName} ->
            let x =
                  -- Uses the same name if not specified
                  fromMaybe varVal assPsurfaceName
             in Map.insert
                  x
                  (EntryBuiltInPersistent varVal (fromStagedPers aPtye))
                  bindingTimeEnv
    )
    ( \varMod (ModuleEntry sigr) bindingTimeEnv ->
        -- Reuses the module name `varMod` in the core language for the surface language.
        Map.insert
          varMod
          (EntryModule (makeBindingTimeEnvFromStub sigr))
          bindingTimeEnv
    )
    Map.empty

handle :: Argument -> IO Bool
handle Argument {inputFilePath, stubFilePath, optimize, distributeIf, displayWidth, compileTimeOnly, fallBackToBindingTime0} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  let lwArg =
        Lwsd.Entrypoint.Argument
          { Lwsd.Entrypoint.inputFilePath = inputFilePath,
            Lwsd.Entrypoint.stubFilePath = stubFilePath,
            Lwsd.Entrypoint.optimize = optimize,
            Lwsd.Entrypoint.distributeIf = distributeIf,
            Lwsd.Entrypoint.displayWidth = displayWidth,
            Lwsd.Entrypoint.compileTimeOnly = compileTimeOnly
          }
  stub <- TextIO.readFile stubFilePath
  let sourceSpecOfStub =
        SourceSpec
          { LocationInFile.source = stub,
            LocationInFile.inputFilePath = stubFilePath
          }
  case LwsdParser.parseBinds sourceSpecOfStub stub of
    Left err -> do
      putSectionLine "parse error of stub:"
      putRenderedLines err
      failure
    Right declsInStub -> do
      (r, stateAfterTraversingStub@TypecheckState {assVarDisplay}) <-
        runReaderT (Lwsd.Entrypoint.typecheckStub sourceSpecOfStub declsInStub) lwArg
      case r of
        Left tyErr -> do
          putSectionLine "type error of stub:"
          putRenderedLines (fmap (Lwsd.Entrypoint.showVar assVarDisplay) tyErr)
          failure
        Right (tyEnvStub, sigr, abinds) -> do
          let initialBindingTimeEnv = makeBindingTimeEnvFromStub sigr
          source <- TextIO.readFile inputFilePath
          let sourceSpecOfInput =
                SourceSpec
                  { LocationInFile.source = source,
                    LocationInFile.inputFilePath = inputFilePath
                  }
          case Parser.parseExpr sourceSpecOfInput source of
            Left err -> do
              putSectionLine "parse error:"
              putRenderedLines err
              failure
            Right e -> do
              putSectionLine "parsed expression:"
              putRenderedLines e
              case BindingTime.analyze sourceSpecOfInput fallBackToBindingTime0 initialBindingTimeEnv e of
                Left analyErr -> do
                  putSectionLine "binding-time analysis error:"
                  putRenderedLines analyErr
                  failure
                Right (bce, lwe) -> do
                  putSectionLine "result of binding-time analysis:"
                  putRenderedLines bce
                  putSectionLine "result of staging:"
                  putRenderedLinesAtStage0 lwe
                  runReaderT (Lwsd.Entrypoint.typecheckAndEvalInput stateAfterTraversingStub sourceSpecOfInput tyEnvStub abinds lwe) lwArg
  where
    putSectionLine :: String -> IO ()
    putSectionLine s = putStrLn ("-------- " ++ s ++ " --------")

    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    failure = return False
