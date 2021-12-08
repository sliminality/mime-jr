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
            BAnd BTrue  BTrue  `denote` e `shouldBe` True
            BAnd BTrue  BFalse `denote` e `shouldBe` False
            BAnd BFalse BTrue  `denote` e `shouldBe` False
            BAnd BFalse BFalse `denote` e `shouldBe` False

            let b = BAnd (BGt (NConst 3) (NConst 4)) 
                         (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` False

            let b = BAnd (BGt (NConst 4) (NConst 3)) 
                         (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` True


        it "disjunction" $ do
            BOr BTrue  BTrue  `denote` e `shouldBe` True
            BOr BTrue  BFalse `denote` e `shouldBe` True
            BOr BFalse BTrue  `denote` e `shouldBe` True
            BOr BFalse BFalse `denote` e `shouldBe` False

            let b = BOr (BGt (NConst 3) (NConst 4)) 
                        (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` True

            let b = BOr (BGt (NConst 4) (NConst 3)) 
                        (BEq (NConst 0) (NConst 0))
            b `denote` e `shouldBe` True

            let b = BOr (BGt (NConst 3) (NConst 3)) 
                        (BEq (NConst 1) (NConst 0))
            b `denote` e `shouldBe` False

        it "negation" $ do
            BNot BTrue  `denote` e `shouldBe` False
            BNot BFalse `denote` e `shouldBe` True

    describe "complex guard" $ do
        it "handles a nontrivial disjunction" $ do
            let b = (NWidth `BGt` NConst 10) 
                    `BOr` (NWidth `BEq` NConst 0)

            let e = Let (Name "myList")
                        (Lit (LStr "abcdefghijklmnopqrstuvwxyz")) $
                    Let (Name "init")
                        (Lit (LNum 0)) $
                    Let (Name "add")
                        (Lam [Name "x", Name "y"] $
                             App (Var (Name "plus")) 
                                 [Var (Name "x"), Var (Name "y")]) $
                        App (Var (Name "foldr")) 
                            [ Var (Name "add")
                            , Var (Name "init")
                            , Var (Name "myList")
                            ]

            b `denote` e `shouldBe` True

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
