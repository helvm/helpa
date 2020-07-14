{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.LPASpec (spec) where

import HelVM.HelPA.Assemblers.LMS

import Test.Hspec

spec :: Spec
spec = do
  describe "strip" $ do
    it "f = g (x - 1);" $ do
      (file "f = g (x - 1);") `shouldBe` Right [Defn (Name "f") (App (Var (Name "g")) (App (App (Var (Name "x")) (Prim Sub)) (Lit 1)))]

    it "g = f (x + 1);" $ do
      (file "g = f (x + 1);") `shouldBe` Right [Defn (Name "g") (App (Var (Name "f")) (App (App (Var (Name "x")) (Prim Add)) (Lit 1)))]

    it "h = \\x y -> (f x) + (g y);" $ do
      (file "h = \\x y -> (f x) + (g y);") `shouldBe` Right [Defn (Name "h") (Lam [Name "x",Name "y"] (App (App (App (Var (Name "f")) (Var (Name "x"))) (Prim Add)) (App (Var (Name "g")) (Var (Name "y")))))]
