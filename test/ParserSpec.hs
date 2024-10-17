module ParserSpec (spec) where

import Data.Functor
import Data.Text (Text)
import Lwsd.Parser qualified as Parser
import Lwsd.Syntax
import Test.Hspec
import Util.TokenUtil (Span (..))

type TypeExpr0 = TypeExprF ()

type Expr0 = ExprF ()

typ :: TypeExprMainF () -> TypeExpr0
typ = TypeExpr ()

tyInt :: TypeExpr0
tyInt = typ (TyName "Int" [])

tyBool :: TypeExpr0
tyBool = typ (TyName "Bool" [])

tyNormalVec :: Expr0 -> TypeExpr0
tyNormalVec e = typ (TyName "Vec" [NormalArg e])

tyPersVec :: Expr0 -> TypeExpr0
tyPersVec e = typ (TyName "Vec" [PersistentArg e])

tyCode :: TypeExpr0 -> TypeExpr0
tyCode = typ . TyCode

tyDepFun :: Var -> TypeExpr0 -> TypeExpr0 -> TypeExpr0
tyDepFun x tye1 tye2 = typ (TyArrow (Just x, tye1) tye2)

tyNondepFun :: TypeExpr0 -> TypeExpr0 -> TypeExpr0
tyNondepFun tye1 tye2 = typ (TyArrow (Nothing, tye1) tye2)

expr :: ExprMainF () -> Expr0
expr = Expr ()

litInt :: Int -> Expr0
litInt = expr . Literal . LitInt

litVec :: [Int] -> Expr0
litVec = expr . Literal . LitVec

var :: Text -> Expr0
var = expr . Var

nonrecLam :: (Var, TypeExpr0) -> Expr0 -> Expr0
nonrecLam binder e = expr (Lam Nothing binder e)

recLam :: (Var, TypeExpr0) -> (Var, TypeExpr0) -> Expr0 -> Expr0
recLam binderF binderX e = expr (Lam (Just binderF) binderX e)

app :: Expr0 -> Expr0 -> Expr0
app e1 e2 = expr (App e1 e2)

binOp :: Var -> Expr0 -> Expr0 -> Expr0
binOp op e1 e2 = app (app (var op) e1) e2

add, sub, mult :: Expr0 -> Expr0 -> Expr0
add = binOp "+"
sub = binOp "-"
mult = binOp "*"

upcast :: Expr0 -> TypeExpr0 -> Expr0
upcast e1 tye2 = expr (As e1 tye2)

bracket :: Expr0 -> Expr0
bracket = expr . Bracket

escape :: Expr0 -> Expr0
escape = expr . Escape

parseExpr :: Text -> Either String Expr0
parseExpr s = fmap void (Parser.parseExpr s)

parseTypeExpr :: Text -> Either String TypeExpr0
parseTypeExpr s = fmap void (Parser.parseTypeExpr s)

exprLoc :: Int -> Int -> ExprMainF Span -> Expr
exprLoc start end = Expr (Span start end)

typLoc :: Int -> Int -> TypeExprMainF Span -> TypeExpr
typLoc start end = TypeExpr (Span start end)

spec :: Spec
spec = do
  describe "Parser.parseExpr (without code locations)" $ do
    it "parses integer literals (1)" $
      parseExpr "42"
        `shouldBe` pure (litInt 42)
    it "parses integer literals (2)" $
      parseExpr "0"
        `shouldBe` pure (litInt 0)
    it "parses vector literals (1)" $
      parseExpr "[| |]"
        `shouldBe` pure (litVec [])
    it "parses vector literals (2)" $
      parseExpr "[| 3; 14; 1592 |]"
        `shouldBe` pure (litVec [3, 14, 1592])
    it "parses variables (1)" $
      parseExpr "x"
        `shouldBe` pure (var "x")
    it "parses variables (2)" $
      parseExpr "foo_bar"
        `shouldBe` pure (var "foo_bar")
    it "parses applications (1)" $
      parseExpr "x y"
        `shouldBe` pure (app (var "x") (var "y"))
    it "parses applications (2)" $
      parseExpr "x y z"
        `shouldBe` pure (app (app (var "x") (var "y")) (var "z"))
    it "parses applications (3)" $
      parseExpr "x (y z)"
        `shouldBe` pure (app (var "x") (app (var "y") (var "z")))
    it "parses applications and integer literals" $
      parseExpr "x 42 z"
        `shouldBe` pure (app (app (var "x") (litInt 42)) (var "z"))
    it "parses lambda abstractions (1)" $
      parseExpr "fun (x : Int) -> x"
        `shouldBe` pure (nonrecLam ("x", tyInt) (var "x"))
    it "parses lambda abstractions (2)" $
      let ty = tyDepFun "n" tyInt tyBool
       in parseExpr "fun (x : (n : Int) -> Bool) -> x y"
            `shouldBe` pure (nonrecLam ("x", ty) (app (var "x") (var "y")))
    it "parses recursive lambda abstractions" $
      parseExpr "rec (self : Int -> Int) -> fun (x : Int) -> x"
        `shouldBe` pure (recLam ("self", tyNondepFun tyInt tyInt) ("x", tyInt) (var "x"))
    it "parses let-expressions" $
      let ty = tyDepFun "n" tyInt tyBool
       in parseExpr "let f = fun (x : (n : Int) -> Bool) -> x y in f"
            `shouldBe` pure (expr (LetIn "f" (nonrecLam ("x", ty) (app (var "x") (var "y"))) (var "f")))
    it "parses if-expressions" $
      parseExpr "if b then x + 1 else x"
        `shouldBe` pure (expr (IfThenElse (var "b") (add (var "x") (litInt 1)) (var "x")))
    it "parses brackets (1)" $
      parseExpr "f &x y"
        `shouldBe` pure (app (app (var "f") (bracket (var "x"))) (var "y"))
    it "parses brackets (2)" $
      parseExpr "f &(g x)"
        `shouldBe` pure (app (var "f") (bracket (app (var "g") (var "x"))))
    it "parses escapes (1)" $
      parseExpr "f ~x y"
        `shouldBe` pure (app (app (var "f") (escape (var "x"))) (var "y"))
    it "parses escapes (2)" $
      parseExpr "f ~(g x)"
        `shouldBe` pure (app (var "f") (escape (app (var "g") (var "x"))))
    it "parses binary operators (1)" $
      parseExpr "2 + 3"
        `shouldBe` pure (add (litInt 2) (litInt 3))
    it "parses binary operators (2)" $
      parseExpr "4 - 3 - 2"
        `shouldBe` pure (sub (sub (litInt 4) (litInt 3)) (litInt 2))
    it "parses binary operators (3)" $
      parseExpr "2 + 3 * 4"
        `shouldBe` pure (add (litInt 2) (mult (litInt 3) (litInt 4)))
    it "parses binary operators (3)" $
      parseExpr "f 2 + 3"
        `shouldBe` pure (add (app (var "f") (litInt 2)) (litInt 3))
    it "parses binary operators (4)" $
      parseExpr "2 + f 3"
        `shouldBe` pure (add (litInt 2) (app (var "f") (litInt 3)))
    it "parses upcasts" $
      parseExpr "[| |] as Vec %n"
        `shouldBe` pure (upcast (litVec []) (tyPersVec (var "n")))
  describe "Parser.parseTypeExpr" $ do
    it "parses dependent function types (1)" $
      parseTypeExpr "(n : Int) -> Bool"
        `shouldBe` pure (tyDepFun "n" tyInt tyBool)
    it "parses dependent function types (2)" $
      parseTypeExpr "(m : Int) -> (n : Int) -> Bool"
        `shouldBe` pure (tyDepFun "m" tyInt (tyDepFun "n" tyInt tyBool))
    it "parses dependent function types (3)" $
      parseTypeExpr "(f : (n : Int) -> Int) -> Bool"
        `shouldBe` pure (tyDepFun "f" (tyDepFun "n" tyInt tyInt) tyBool)
    it "parses non-dependent function types (1)" $
      parseTypeExpr "Int -> Bool"
        `shouldBe` pure (tyNondepFun tyInt tyBool)
    it "parses non-dependent function types (2)" $
      parseTypeExpr "Int -> Int -> Bool"
        `shouldBe` pure (tyNondepFun tyInt (tyNondepFun tyInt tyBool))
    it "parses non-dependent function types (3)" $
      parseTypeExpr "(Int -> Int) -> Bool"
        `shouldBe` pure (tyNondepFun (tyNondepFun tyInt tyInt) tyBool)
    it "parses mixed function types (1)" $
      parseTypeExpr "(m : Int) -> Int -> Bool"
        `shouldBe` pure (tyDepFun "m" tyInt (tyNondepFun tyInt tyBool))
    it "parses mixed function types (2)" $
      parseTypeExpr "Int -> (n : Int) -> Bool"
        `shouldBe` pure (tyNondepFun tyInt (tyDepFun "n" tyInt tyBool))
    it "parses mixed function types (3)" $
      parseTypeExpr "(f : Int -> Int) -> Bool"
        `shouldBe` pure (tyDepFun "f" (tyNondepFun tyInt tyInt) tyBool)
    it "parses mixed function types (4)" $
      parseTypeExpr "((n : Int) -> Int) -> Bool"
        `shouldBe` pure (tyNondepFun (tyDepFun "n" tyInt tyInt) tyBool)
    it "parses type applications (1)" $
      parseTypeExpr "Vec n"
        `shouldBe` pure (tyNormalVec (var "n"))
    it "parses type applications (2)" $
      parseTypeExpr "Vec %n"
        `shouldBe` pure (tyPersVec (var "n"))
    it "parses type applications (3)" $
      parseTypeExpr "(v : Vec n) -> Bool"
        `shouldBe` pure (tyDepFun "v" (tyNormalVec (var "n")) tyBool)
    it "parses type applications (4)" $
      parseTypeExpr "Vec (succ n)"
        `shouldBe` pure (tyNormalVec (app (var "succ") (var "n")))
    it "parses type applications (5)" $
      parseTypeExpr "Vec %(succ n)"
        `shouldBe` pure (tyPersVec (app (var "succ") (var "n")))
    it "parses code types (1)" $
      parseTypeExpr "&Int"
        `shouldBe` pure (tyCode tyInt)
    it "parses code types (2)" $
      parseTypeExpr "&(Vec n)"
        `shouldBe` pure (tyCode (tyNormalVec (var "n")))
    it "parses code types (3)" $
      parseTypeExpr "&Int -> Bool"
        `shouldBe` pure (tyNondepFun (tyCode tyInt) tyBool)
    it "parses code types (4)" $
      parseTypeExpr "&(Int -> Bool)"
        `shouldBe` pure (tyCode (tyNondepFun tyInt tyBool))
  describe "Parser.parseExpr (with code locations)" $ do
    it "parses integer literals" $
      Parser.parseExpr "42"
        `shouldBe` pure (exprLoc 0 2 $ Literal (LitInt 42))
    it "parses vector literals" $
      Parser.parseExpr "[| 3; 14; 1592 |]"
        `shouldBe` pure (exprLoc 0 17 $ Literal (LitVec [3, 14, 1592]))
    it "parses matrix literals" $
      Parser.parseExpr "[# 3, 14; 159, 2; 653, 5 #]"
        `shouldBe` pure (exprLoc 0 27 $ Literal (LitMat [[3, 14], [159, 2], [653, 5]]))
    it "parses variables" $
      Parser.parseExpr "foo_bar"
        `shouldBe` pure (exprLoc 0 7 $ Var "foo_bar")
    it "parses applications (1)" $
      Parser.parseExpr "x y"
        `shouldBe` pure (exprLoc 0 3 $ App (exprLoc 0 1 $ Var "x") (exprLoc 2 3 $ Var "y"))
    it "parses applications (2)" $
      let e =
            exprLoc 0 5 $
              App
                (exprLoc 0 3 $ App (exprLoc 0 1 $ Var "x") (exprLoc 2 3 $ Var "y"))
                (exprLoc 4 5 $ Var "z")
       in Parser.parseExpr "x y z"
            `shouldBe` pure e
    it "parses applications (3)" $
      let e =
            exprLoc 0 7 $
              App
                (exprLoc 0 1 $ Var "x")
                (exprLoc 2 7 $ App (exprLoc 3 4 $ Var "y") (exprLoc 5 6 $ Var "z"))
       in Parser.parseExpr "x (y z)" `shouldBe` pure e
    it "parses brackets" $
      let e =
            exprLoc 0 8 $
              App
                (exprLoc 0 1 $ Var "f")
                (exprLoc 2 8 $ Bracket (exprLoc 3 8 $ App (exprLoc 4 5 $ Var "g") (exprLoc 6 7 $ Var "x")))
       in Parser.parseExpr "f &(g x)"
            `shouldBe` pure e
    it "parses lambda abstractions" $
      let ty =
            typLoc 9 26 $
              TyArrow
                (Just "n", typLoc 14 17 $ TyName "Int" [])
                (typLoc 22 26 $ TyName "Bool" [])
          e =
            exprLoc 0 34 $
              Lam
                Nothing
                ("x", ty)
                (exprLoc 31 34 $ App (exprLoc 31 32 $ Var "x") (exprLoc 33 34 $ Var "y"))
       in Parser.parseExpr "fun (x : (n : Int) -> Bool) -> x y"
            `shouldBe` pure e
