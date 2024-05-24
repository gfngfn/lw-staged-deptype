module LibMain (handle) where

import Control.Monad.Trans.State
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Evaluator qualified
import Formatter qualified
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

initialEnv :: Env0
initialEnv =
  List.foldl'
    (flip (uncurry Map.insert))
    Map.empty
    [ ("add", clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIAdd "x1" "x2")))),
      ("gen_vadd", clo "x1" tyValInt (A0AppBuiltIn (BIGenVadd "x1"))),
      ("gen_vconcat", clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIGenVconcat "x1" "x2"))))
    ]
  where
    -- TODO: extend this with `gen_vadd` and `gen_vconcat`

    clo x a0tyv1 a0tye2 = A0ValLam (x, a0tyv1) a0tye2 initialEnv
    tyValInt = A0TyValPrim A0TyValInt
    lam x a0tye1 = A0Lam (x, a0tye1)
    tyInt = A0TyPrim A0TyInt

handle :: String -> IO ()
handle inputFilePath = do
  source <- TextIO.readFile inputFilePath
  case Parser.parseExpr source of
    Left err -> do
      putStrLn "-------- parse error: --------"
      putStrLn err
    Right e -> do
      case evalStateT (Typechecker.typecheckExpr1 id initialTypeEnv e) () of
        Left (tyErr, _travMod) -> do
          putStrLn "-------- type error: --------"
          putStrLn $ Text.unpack $ Formatter.render tyErr
        Right (a1tye, a1e) -> do
          putStrLn "-------- type: --------"
          putStrLn $ Text.unpack $ Formatter.render a1tye
          putStrLn "-------- expression: --------"
          putStrLn $ Text.unpack $ Formatter.render a1e
          case Evaluator.evalExpr1 initialEnv a1e of
            Left err -> do
              putStrLn "-------- eval error: --------"
              print err
            Right a1v -> do
              putStrLn "-------- expanded expression: --------"
              putStrLn $ Text.unpack $ Formatter.render a1v
