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
    [ ("add", tyInt --> tyInt --> tyInt),
      ("gen_vadd", tyGenVadd),
      ("gen_vconcat", tyGenVconcat)
    ]
  where
    tyGenVadd :: Ass0TypeExpr
    tyGenVadd =
      ("a", tyInt)
        -:> A0TyCode (tyVec (A0Var "a") ==> tyVec (A0Var "a") ==> tyVec (A0Var "a"))

    tyGenVconcat :: Ass0TypeExpr
    tyGenVconcat =
      ("a", tyInt)
        -:> ("b", tyInt)
        -:> A0TyCode
          ( tyVec (A0Var "a")
              ==> tyVec (A0Var "b")
              ==> tyVec (A0App (A0App (A0Var "add") (A0Var "a")) (A0Var "b"))
          )

    tyInt :: Ass0TypeExpr
    tyInt = A0TyPrim A0TyInt

    tyVec :: Ass0Expr -> Ass1TypeExpr
    tyVec = A1TyPrim . A1TyVec

    (-->) :: Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr
    (-->) a0tye1 = A0TyArrow (Nothing, a0tye1)
    infixr 0 -->

    (-:>) :: (Var, Ass0TypeExpr) -> Ass0TypeExpr -> Ass0TypeExpr
    (-:>) (x, a0tye1) = A0TyArrow (Just x, a0tye1)
    infixr 0 -:>

    (==>) :: Ass1TypeExpr -> Ass1TypeExpr -> Ass1TypeExpr
    (==>) = A1TyArrow
    infixr 0 ==>

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
