module ParserSpec (spec) where

import Parser
import Syntax
import Test.Hspec

tyInt :: TypeExpr
tyInt = TyName "int" []

tyBool :: TypeExpr
tyBool = TyName "bool" []

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
      Parser.parseExpr "fun (x : int) -> x"
        `shouldBe` pure (Lam ("x", tyInt) (Var "x"))
    it "parses lambda abstractions (2)" $
      let ty = TyArrow ("n", tyInt) tyBool
       in Parser.parseExpr "fun (x : (n : int) -> bool) -> x y"
            `shouldBe` pure (Lam ("x", ty) (App (Var "x") (Var "y")))
  describe "Parser.parseTypeExpr" $ do
    it "parses function types (1)" $
      Parser.parseTypeExpr"(n : int) -> bool"
        `shouldBe` pure (TyArrow ("n", tyInt) tyBool)
    it "parses function types (2)" $
      Parser.parseTypeExpr"(m : int) -> (n : int) -> bool"
        `shouldBe` pure (TyArrow ("m", tyInt) (TyArrow ("n", tyInt) tyBool))
    it "parses function types (3)" $
      Parser.parseTypeExpr"(f : (n : int) -> int) -> bool"
        `shouldBe` pure (TyArrow ("f", TyArrow ("n", tyInt) tyInt) tyBool)
    it "parses type applications (1)" $
      Parser.parseTypeExpr"vec n"
        `shouldBe` pure (TyName "vec" [Var "n"])
    it "parses type applications (2)" $
      Parser.parseTypeExpr"(v : vec n) -> bool"
        `shouldBe` pure (TyArrow ("v", TyName "vec" [Var "n"]) tyBool)
    it "parses type applications (3)" $
      Parser.parseTypeExpr"vec (succ n)"
        `shouldBe` pure (TyName "vec" [App (Var "succ") (Var "n")])
