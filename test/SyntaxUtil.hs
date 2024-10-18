module SyntaxUtil where

import Data.Text (Text)
import Lwsd.Syntax

type TypeExprVoid = TypeExprF ()

type ExprVoid = ExprF ()

typ :: TypeExprMainF () -> TypeExprVoid
typ = TypeExpr ()

tyInt :: TypeExprVoid
tyInt = typ (TyName "Int" [])

tyBool :: TypeExprVoid
tyBool = typ (TyName "Bool" [])

tyNormalVec :: ExprVoid -> TypeExprVoid
tyNormalVec e = typ (TyName "Vec" [NormalArg e])

tyPersVec :: ExprVoid -> TypeExprVoid
tyPersVec e = typ (TyName "Vec" [PersistentArg e])

tyCode :: TypeExprVoid -> TypeExprVoid
tyCode = typ . TyCode

tyDepFun :: Var -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyDepFun x tye1 tye2 = typ (TyArrow (Just x, tye1) tye2)

tyNondepFun :: TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyNondepFun tye1 tye2 = typ (TyArrow (Nothing, tye1) tye2)

expr :: ExprMainF () -> ExprVoid
expr = Expr ()

litInt :: Int -> ExprVoid
litInt = expr . Literal . LitInt

litVec :: [Int] -> ExprVoid
litVec = expr . Literal . LitVec

var :: Text -> ExprVoid
var = expr . Var

nonrecLam :: (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
nonrecLam binder e = expr (Lam Nothing binder e)

recLam :: (Var, TypeExprVoid) -> (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
recLam binderF binderX e = expr (Lam (Just binderF) binderX e)

app :: ExprVoid -> ExprVoid -> ExprVoid
app e1 e2 = expr (App e1 e2)

binOp :: Var -> ExprVoid -> ExprVoid -> ExprVoid
binOp op e1 e2 = app (app (var op) e1) e2

add, sub, mult :: ExprVoid -> ExprVoid -> ExprVoid
add = binOp "+"
sub = binOp "-"
mult = binOp "*"

upcast :: ExprVoid -> TypeExprVoid -> ExprVoid
upcast e1 tye2 = expr (As e1 tye2)

bracket :: ExprVoid -> ExprVoid
bracket = expr . Bracket

escape :: ExprVoid -> ExprVoid
escape = expr . Escape

a0litInt :: Int -> Ass0Expr
a0litInt n = A0Literal (ALitInt n)

a0var :: Text -> Ass0Expr
a0var = A0Var

a0bracket :: Ass1Expr -> Ass0Expr
a0bracket = A0Bracket

a1var :: Text -> Ass1Expr
a1var = A1Var
