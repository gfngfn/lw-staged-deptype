module LibMain (handle) where

import Control.Monad.Trans.State
import Data.List qualified as List
import Data.Text.IO qualified as TextIO
import Parser
import Syntax
import TypeEnv (TypeEnv)
import TypeEnv qualified
import Typechecker qualified
import Prelude

initialTypeEnv :: TypeEnv
initialTypeEnv =
  List.foldl'
    (\tyEnv (x, a0tye) -> TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye) tyEnv)
    TypeEnv.empty
    [ ("add", tyInt --> (tyInt --> tyInt)) ]
  where
    tyInt :: Ass0TypeExpr
    tyInt = A0TyName "Int" []

    (-->) :: Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr
    (-->) a0tye1 = A0TyArrow (Nothing, a0tye1)

handle :: String -> IO ()
handle inputFilePath = do
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn $ "parse error: " ++ err
    Right e -> do
      case evalStateT (Typechecker.typecheckExpr1 id initialTypeEnv e) () of
        Left (tyErr, _travMod) -> do
          putStrLn "-------- type error: --------"
          print tyErr
        Right atye -> do
          putStrLn "-------- type: --------"
          print atye
