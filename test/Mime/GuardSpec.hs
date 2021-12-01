{-# LANGUAGE OverloadedStrings #-}

module Mime.GuardSpec (main, spec) where

import Test.Hspec
import Mime.Grammar
import Mime.Guard

bspec :: Spec 
bspec = do
    describe "inequalities" $ do
        let e = Lit (LNum 0)

        it "handles constants" $ do
            let b = BGt (NConst 3) (NConst 4)
            b `denote` e `shouldBe` False

            let b = BGt (NConst 4) (NConst 3)
            b `denote` e `shouldBe` True

        it "greater than is strict" $ do
            let b = BGt (NConst 3) (NConst 3) 
            b `denote` e `shouldBe` False

        it "handles widths" $ do
            let b = BEq (NConst 24) NWidth
            let e = Let (Name "f") (Lam [Name "x"] (Var (Name "x"))) $
                    Let (Name "g") (Lam [Name "y"] (Var (Name "y"))) $
                         App (Var (Name "f")) [ Var (Name "g")
                                       , Lit (LNum 111111111111111111) ]
            b `denote` e `shouldBe` True

    describe "logical operators" $ do
        let e = Lit (LNum 0)

        it "conjunction" $ do
            let b = BAnd (BGt (NConst 3) (NConst 4)) 
                         (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` False

            let b = BAnd (BGt (NConst 4) (NConst 3)) 
                         (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` True

        it "disjunction" $ do
            let b = BOr (BGt (NConst 3) (NConst 4)) 
                        (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` True

            let b = BOr (BGt (NConst 4) (NConst 3)) 
                        (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` True

            let b = BOr (BGt (NConst 3) (NConst 3)) 
                        (BEq (NConst 1) (NConst 0))
            b `denote` e `shouldBe` False


nspec :: Spec
nspec = do
    it "handles width" $ do
        let b = NWidth
        let e = Lit (LNum 0)
        b `denote` e `shouldBe` 1

        let b = NWidth
        let e = Let (Name "f") (Lam [Name "x"] (Var (Name "x"))) $
                Let (Name "g") (Lam [Name "y"] (Var (Name "y"))) $
                     App (Var (Name "f")) [ Var (Name "g")
                                   , Lit (LNum 111111111111111111) ]
        b `denote` e `shouldBe` 24

spec :: Spec
spec = do
    describe "booleans" bspec
    describe "naturals" nspec

main :: IO ()
main = hspec spec
