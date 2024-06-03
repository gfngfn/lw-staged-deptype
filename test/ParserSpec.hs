module ParserSpec (spec) where

import Data.Functor
import Data.Text (Text)
import Lwsd.Parser qualified as Parser
import Lwsd.Syntax
import Lwsd.Token (Span (..))
import Test.Hspec

type TypeExpr0 = TypeExprF ()

type Expr0 = ExprF ()

typ :: TypeExprMain () -> TypeExpr0
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

expr :: ExprMain () -> Expr0
expr = Expr ()

litInt :: Int -> Expr0
litInt = expr . Literal . LitInt

litVec :: [Int] -> Expr0
litVec = expr . Literal . LitVec

var :: Text -> Expr0
var = expr . Var

lam :: (Var, TypeExpr0) -> Expr0 -> Expr0
lam binder e = expr (Lam binder e)

app :: Expr0 -> Expr0 -> Expr0
app e1 e2 = expr (App e1 e2)

bracket :: Expr0 -> Expr0
bracket = expr . Bracket

escape :: Expr0 -> Expr0
escape = expr . Escape

parseExpr :: Text -> Either String Expr0
parseExpr s = fmap void (Parser.parseExpr s)

parseTypeExpr :: Text -> Either String TypeExpr0
parseTypeExpr s = fmap void (Parser.parseTypeExpr s)

exprLoc :: Int -> Int -> ExprMain Span -> Expr
exprLoc start end = Expr (Span start end)

typLoc :: Int -> Int -> TypeExprMain Span -> TypeExpr
typLoc start end = TypeExpr (Span start end)

spec :: Spec
spec = do
  describe "Parser.parseExpr (without code locations)" $ do
    it "parses integer literals" $
      parseExpr "42"
        `shouldBe` pure (litInt 42)
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
        `shouldBe` pure (lam ("x", tyInt) (var "x"))
    it "parses lambda abstractions (2)" $
      let ty = tyDepFun "n" tyInt tyBool
       in parseExpr "fun (x : (n : Int) -> Bool) -> x y"
            `shouldBe` pure (lam ("x", ty) (app (var "x") (var "y")))
    it "parses let expressions" $
      let ty = tyDepFun "n" tyInt tyBool
       in parseExpr "let f = fun (x : (n : Int) -> Bool) -> x y in f"
            `shouldBe` pure (expr (LetIn "f" (lam ("x", ty) (app (var "x") (var "y"))) (var "f")))
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
                ("x", ty)
                (exprLoc 31 34 $ App (exprLoc 31 32 $ Var "x") (exprLoc 33 34 $ Var "y"))
       in Parser.parseExpr "fun (x : (n : Int) -> Bool) -> x y"
            `shouldBe` pure e
