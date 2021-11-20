{-# LANGUAGE OverloadedStrings #-}

module Mime.GrammarSpec (main, spec) where

import Test.Hspec
import Mime.Grammar (repr, Expr (..), Lit (..))

spec :: Spec
spec = describe "repr" $ do
    it "represents a simple application" $ do
        let expr = Let "f" (Lam "x" (Var "x")) $
                       App (Var "f") (Lit (LNum 1))
        repr expr
            `shouldBe` "let f = λx.x in" 
                <> "\n" <> "f 1"

    it "represents nested let bindings" $ do
        let expr = Let "f" (Lam "x" (Var "x")) $
                   Let "g" (Lam "y" (Var "y")) $
                        App (App (Var "f") (Var "g")) $
                        Lit (LNum 1)
        repr expr 
            `shouldBe` "let f = λx.x in" 
                <> "\n" <> "let g = λy.y in" 
                <> "\n" <> "f g 1"


main :: IO ()
main = hspec spec
