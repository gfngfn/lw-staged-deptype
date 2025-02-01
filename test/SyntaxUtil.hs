module SyntaxUtil where

import Data.Text (Text)
import Lwsd.SrcSyntax
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
tyNormalVec e = typ (TyName "Vec" [ExprArgNormal e])

tyPersVec :: ExprVoid -> TypeExprVoid
tyPersVec e = typ (TyName "Vec" [ExprArgPersistent e])

tyCode :: TypeExprVoid -> TypeExprVoid
tyCode = typ . TyCode

tyDepFun :: Var -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyDepFun x tye1 tye2 = typ (TyArrow (Just x, tye1) tye2)

tyNondepFun :: TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyNondepFun tye1 tye2 = typ (TyArrow (Nothing, tye1) tye2)

tyRefinement :: Var -> TypeExprVoid -> ExprVoid -> TypeExprVoid
tyRefinement x tye1 e2 = typ (TyRefinement x tye1 e2)

expr :: ExprMainF () -> ExprVoid
expr = Expr ()

litInt :: Int -> ExprVoid
litInt = expr . Literal . LitInt

litFloat :: Double -> ExprVoid
litFloat = expr . Literal . LitFloat

litList :: [ExprVoid] -> ExprVoid
litList = expr . Literal . LitList

litVec :: [Int] -> ExprVoid
litVec = expr . Literal . LitVec

short :: Var -> ExprMainF ann
short x = Var ([], x)

long :: [Var] -> Var -> ExprMainF ann
long ms x = Var (ms, x)

var :: Var -> ExprVoid
var = expr . short

longVar :: [Var] -> Var -> ExprVoid
longVar ms x = expr (long ms x)

nonrecLam :: (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
nonrecLam binder e = expr (Lam Nothing binder e)

recLam :: (Var, TypeExprVoid) -> (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
recLam binderF binderX e = expr (Lam (Just binderF) binderX e)

app :: ExprVoid -> ExprVoid -> ExprVoid
app e1 e2 = expr (App e1 e2)

appOptGiven :: ExprVoid -> ExprVoid -> ExprVoid
appOptGiven e1 e2 = expr (AppOptGiven e1 e2)

binOp :: Var -> ExprVoid -> ExprVoid -> ExprVoid
binOp op e1 = app (app (var op) e1)

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
a0var = A0Var . AssVar

a0app :: Ass0Expr -> Ass0Expr -> Ass0Expr
a0app = A0App

a0nonrecLam :: Text -> StrictAss0TypeExpr -> Ass0Expr -> Ass0Expr
a0nonrecLam x sa0tye1 = A0Lam Nothing (AssVar x, sa0tye1)

a0recLam :: Text -> StrictAss0TypeExpr -> Text -> StrictAss0TypeExpr -> Ass0Expr -> Ass0Expr
a0recLam f sa0tyeRec x sa0tye1 = A0Lam (Just (AssVar f, sa0tyeRec)) (AssVar x, sa0tye1)

a0bracket :: Ass1Expr -> Ass0Expr
a0bracket = A0Bracket

sa0tyInt :: StrictAss0TypeExpr
sa0tyInt = SA0TyPrim A0TyInt Nothing

sa0nondepTyArrow :: StrictAss0TypeExpr -> StrictAss0TypeExpr -> StrictAss0TypeExpr
sa0nondepTyArrow sa0tye1 = SA0TyArrow (Nothing, sa0tye1)

a1var :: Text -> Ass1Expr
a1var = A1Var . AssVar

a1app :: Ass1Expr -> Ass1Expr -> Ass1Expr
a1app = A1App

a1nonrecLam :: Text -> Ass1TypeExpr -> Ass1Expr -> Ass1Expr
a1nonrecLam x a1tye1 = A1Lam Nothing (AssVar x, a1tye1)

a1escape :: Ass0Expr -> Ass1Expr
a1escape = A1Escape

a1tyInt :: Ass1TypeExpr
a1tyInt = A1TyPrim A1TyInt

a1tyVec :: Ass0Expr -> Ass1TypeExpr
a1tyVec a0e = A1TyPrim (a1TyVec a0e)
