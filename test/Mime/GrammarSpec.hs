{-# LANGUAGE OverloadedStrings #-}

module Mime.GrammarSpec (main, spec) where

import Data.Text (Text)
import Test.Hspec
import Mime.Document (IntoDoc (..))
import Mime.Grammar (Expr (..), Lit (..), Name (..))

reprSpec :: Spec
reprSpec = do
    it "represents a simple application" $ do
        let e = Let (Name "f") (Lam [Name "x"] (Var (Name "x"))) $
                    App (Var (Name "f")) [Lit (LNum 1)]
        (repr e :: Text) `shouldBe`
            "let f = \\x -> x in\n\
            \    f 1"

    it "represents nested let bindings" $ do
        let e = Let (Name "f") (Lam [Name "x"] (Var (Name "x"))) $
                Let (Name "g") (Lam [Name "y"] (Var (Name "y"))) $
                     App (Var (Name "f")) [ Var (Name "g")
                                   , Lit (LNum 1) ]

        (repr e :: Text) `shouldBe`
            "let f = \\x -> x in\n\
            \let g = \\y -> y in\n\
            \    f g\n\
            \      1"

    it "represents nested parenthesized expressions" $ do
        let e = Let (Name "id") (Lam [Name "x"] (Var (Name "x"))) $
                App (Var (Name "id"))
                    [ Par (Let (Name "id2") (Lam [Name "y"] (Var (Name "y"))) $
                        App (Var (Name "id")) [Var (Name "id2")])
                    , Lit (LNum 433)]

        (repr e :: Text) `shouldBe`
            "let id = \\x -> x in\n\
            \    id (let id2 = \\y -> y in\n\
            \            id id2)\n\
            \       433" 

    it "represents nested lets" $ do 
        let e = Let (Name "outer")
                    (Par (Let (Name "inner") (Lit (LNum 111))
                              (Lit (LNum 1))))
                    (Lit (LNum 22))

        (repr e :: Text) `shouldBe`
            "let outer = (let inner = 111 in\n\
            \                 1) in\n\
            \    22"

widthSpec :: Spec
widthSpec = do
    it "computes multi-line width" $ do
        let e = Let (Name "f") (Lam [Name "x"] (Var (Name "x"))) $
                Let (Name "g") (Lam [Name "y"] (Var (Name "y"))) $
                     App (Var (Name "f")) [ Var (Name "g")
                                   , Lit (LNum 111111111111111111) ]

        width e `shouldBe` 24
            -- "let f = \\x -> x in\n\
            -- \let g = \\y -> y in\n\
            -- \    f g\n\
            -- \      111111111111111111"

        let e = Let (Name "id") (Lam [Name "x"] (Var (Name "x"))) $
                App (Var (Name "id"))
                    [ Par (Let (Name "id2") (Lam [Name "y"] (Var (Name "y"))) $
                        App (Var (Name "id")) [Var (Name "id2")])
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
