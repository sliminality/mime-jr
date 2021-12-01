{-# LANGUAGE OverloadedStrings #-}

module Mime.GrammarSpec (main, spec) where

import Data.Text (Text)
import Test.Hspec
import Mime.Document (IntoDoc (..))
import Mime.Grammar (Expr (..), Lit (..))

reprSpec :: Spec
reprSpec = do
    it "represents a simple application" $ do
        let e = Let "f" (Lam ["x"] (Var "x")) $
                    App (Var "f") [(Lit (LNum 1))]
        (repr e :: Text) `shouldBe`
            "let f = \\x -> x in\n\
            \    f 1"

    it "represents nested let bindings" $ do
        let e = Let "f" (Lam ["x"] (Var "x")) $
                Let "g" (Lam ["y"] (Var "y")) $
                     App (Var "f") [ (Var "g")
                                   , Lit (LNum 1) ]

        (repr e :: Text) `shouldBe`
            "let f = \\x -> x in\n\
            \let g = \\y -> y in\n\
            \    f g\n\
            \      1"

    it "represents nested parenthesized expressions" $ do
        let e = Let "id" (Lam ["x"] (Var "x")) $
                App (Var "id")
                    [ (Par (Let "id2" (Lam ["y"] (Var "y")) $
                                       App (Var "id") [(Var "id2")]))
                    , Lit (LNum 433)]

        (repr e :: Text) `shouldBe`
            "let id = \\x -> x in\n\
            \    id (let id2 = \\y -> y in\n\
            \            id id2)\n\
            \       433" 

widthSpec :: Spec
widthSpec = do
    it "computes multi-line width" $ do
        let e = Let "f" (Lam ["x"] (Var "x")) $
                Let "g" (Lam ["y"] (Var "y")) $
                     App (Var "f") [ (Var "g")
                                   , Lit (LNum 111111111111111111) ]

        width e `shouldBe` 24
            -- "let f = \\x -> x in\n\
            -- \let g = \\y -> y in\n\
            -- \    f g\n\
            -- \      111111111111111111"

        let e = Let "id" (Lam ["x"] (Var "x")) $
                App (Var "id")
                    [ (Par (Let "id2" (Lam ["y"] (Var "y")) $
                                       App (Var "id") [(Var "id2")]))
                    , Lit (LNum 433)]

        width e `shouldBe` 28
            -- "let id = \\x -> x in\n\
            -- \    id (let id2 = \\y -> y in\n\
            -- \            id id2)\n\
            -- \       433" 

spec :: Spec
spec = do
    describe "repr"  reprSpec
    describe "width" widthSpec

main :: IO ()
main = hspec spec
