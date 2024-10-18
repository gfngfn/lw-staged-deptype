module SyntaxUtil where

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

var :: Var -> ExprVoid
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

a0var :: Var -> Ass0Expr
a0var = A0Var

a0app :: Ass0Expr -> Ass0Expr -> Ass0Expr
a0app = A0App

a0nonrecLam :: Var -> Ass0TypeExpr -> Ass0Expr -> Ass0Expr
a0nonrecLam x a0tye1 = A0Lam Nothing (x, a0tye1)

a0recLam :: Var -> Ass0TypeExpr -> Var -> Ass0TypeExpr -> Ass0Expr -> Ass0Expr
a0recLam f a0tyeRec x a0tye1 = A0Lam (Just (f, a0tyeRec)) (x, a0tye1)

a0bracket :: Ass1Expr -> Ass0Expr
a0bracket = A0Bracket

a0tyInt :: Ass0TypeExpr
a0tyInt = A0TyPrim A0TyInt

a0nondepTyArrow :: Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr
a0nondepTyArrow a0tye1 = A0TyArrow (Nothing, a0tye1)

a1var :: Var -> Ass1Expr
a1var = A1Var

a1app :: Ass1Expr -> Ass1Expr -> Ass1Expr
a1app = A1App

a1nonrecLam :: Var -> Ass1TypeExpr -> Ass1Expr -> Ass1Expr
a1nonrecLam x a1tye1 = A1Lam Nothing (x, a1tye1)

a1escape :: Ass0Expr -> Ass1Expr
a1escape = A1Escape

a1tyInt :: Ass1TypeExpr
a1tyInt = A1TyPrim A1TyInt

a1tyVec :: Ass0Expr -> Ass1TypeExpr
a1tyVec a0e = A1TyPrim (A1TyVec a0e)
