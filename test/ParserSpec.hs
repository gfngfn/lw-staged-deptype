module ParserSpec (spec) where

import Parser
import Syntax
import Test.Hspec

tyInt :: TypeExpr
tyInt = TyName "Int" []

tyBool :: TypeExpr
tyBool = TyName "Bool" []

tyVec :: Expr -> TypeExpr
tyVec e = TyName "Vec" [e]

tyDepFun :: Var -> TypeExpr -> TypeExpr -> TypeExpr
tyDepFun x tye1 = TyArrow (Just x, tye1)

tyNondepFun :: TypeExpr -> TypeExpr -> TypeExpr
tyNondepFun tye1 = TyArrow (Nothing, tye1)

spec :: Spec
spec = do
  describe "Parser.parseExpr" $ do
    it "parses variables (1)" $
      Parser.parseExpr "x"
        `shouldBe` pure (Var "x")
    it "parses variables (2)" $
      Parser.parseExpr "foo_bar"
        `shouldBe` pure (Var "foo_bar")
    it "parses applications (1)" $
      Parser.parseExpr "x y"
        `shouldBe` pure (App (Var "x") (Var "y"))
    it "parses applications (2)" $
      Parser.parseExpr "x y z"
        `shouldBe` pure (App (App (Var "x") (Var "y")) (Var "z"))
    it "parses applications (3)" $
      Parser.parseExpr "x (y z)"
        `shouldBe` pure (App (Var "x") (App (Var "y") (Var "z")))
    it "parses lambda abstractions (1)" $
      Parser.parseExpr "fun (x : Int) -> x"
        `shouldBe` pure (Lam ("x", tyInt) (Var "x"))
    it "parses lambda abstractions (2)" $
      let ty = tyDepFun "n" tyInt tyBool
       in Parser.parseExpr "fun (x : (n : Int) -> Bool) -> x y"
            `shouldBe` pure (Lam ("x", ty) (App (Var "x") (Var "y")))
  describe "Parser.parseTypeExpr" $ do
    it "parses dependent function types (1)" $
      Parser.parseTypeExpr "(n : Int) -> Bool"
        `shouldBe` pure (tyDepFun "n" tyInt tyBool)
    it "parses dependent function types (2)" $
      Parser.parseTypeExpr "(m : Int) -> (n : Int) -> Bool"
        `shouldBe` pure (tyDepFun "m" tyInt (tyDepFun "n" tyInt tyBool))
    it "parses dependent function types (3)" $
      Parser.parseTypeExpr "(f : (n : Int) -> Int) -> Bool"
        `shouldBe` pure (tyDepFun "f" (tyDepFun "n" tyInt tyInt) tyBool)
    it "parses non-dependent function types (1)" $
      Parser.parseTypeExpr "Int -> Bool"
        `shouldBe` pure (tyNondepFun tyInt tyBool)
    it "parses non-dependent function types (2)" $
      Parser.parseTypeExpr "Int -> Int -> Bool"
        `shouldBe` pure (tyNondepFun tyInt (tyNondepFun tyInt tyBool))
    it "parses non-dependent function types (3)" $
      Parser.parseTypeExpr "(Int -> Int) -> Bool"
        `shouldBe` pure (tyNondepFun (tyNondepFun tyInt tyInt) tyBool)
    it "parses mixed function types (1)" $
      Parser.parseTypeExpr "(m : Int) -> Int -> Bool"
        `shouldBe` pure (tyDepFun "m" tyInt (tyNondepFun tyInt tyBool))
    it "parses mixed function types (2)" $
      Parser.parseTypeExpr "Int -> (n : Int) -> Bool"
        `shouldBe` pure (tyNondepFun tyInt (tyDepFun "n" tyInt tyBool))
    it "parses mixed function types (3)" $
      Parser.parseTypeExpr "(f : Int -> Int) -> Bool"
        `shouldBe` pure (tyDepFun "f" (tyNondepFun tyInt tyInt) tyBool)
    it "parses mixed function types (4)" $
      Parser.parseTypeExpr "((n : Int) -> Int) -> Bool"
        `shouldBe` pure (tyNondepFun (tyDepFun "n" tyInt tyInt) tyBool)
    it "parses type applications (1)" $
      Parser.parseTypeExpr "Vec n"
        `shouldBe` pure (tyVec (Var "n"))
    it "parses type applications (2)" $
      Parser.parseTypeExpr "(v : Vec n) -> Bool"
        `shouldBe` pure (tyDepFun "v" (tyVec (Var "n")) tyBool)
    it "parses type applications (3)" $
      Parser.parseTypeExpr "Vec (succ n)"
        `shouldBe` pure (tyVec (App (Var "succ") (Var "n")))
