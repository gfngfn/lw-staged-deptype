module LibMain (handle) where

import Control.Monad.Trans.State
import Data.Text.IO qualified as TextIO
import Parser
import TypeEnv qualified
import Typechecker qualified
import Prelude hiding (mod)

handle :: String -> IO ()
handle inputFilePath = do
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn $ "parse error: " ++ err
    Right e -> do
      case evalStateT (Typechecker.typecheckExpr1 id TypeEnv.empty e) () of
        Left (tyErr, _travMod) -> do
          putStrLn "-------- type error: --------"
          print tyErr
        Right atye -> do
          putStrLn "-------- type: --------"
          print atye
