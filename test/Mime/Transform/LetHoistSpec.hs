{-# LANGUAGE OverloadedStrings #-}

module Mime.Transform.LetHoistSpec (main, spec) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Test.Hspec
import Mime.Document (IntoDoc (..))
import Mime.Grammar
import Mime.Transform.LetHoist (getBindings, LetState (..), Argument (..))

spec :: Spec 
spec = describe "getBindings" $ do
    it "traverses sub-expressions" $ do 
        {- 
           let id = \x -> x in
               id (let id2 = \y -> y 162 in
                       id id2)
                  433
        -}
        let e = Let (Name "id") (Lam [Name "x"] (Var (Name "x"))) $
                App (Var (Name "id"))
                    [ Par (Let (Name "id2") 
                               (Lam [Name "y"] 
                                    (App (Var (Name "y"))
                                         [ Lit (LNum 162) ])) $
                           App (Var (Name "id")) [Var (Name "id2")])
                    , Lit (LNum 433) ]

        let result = getBindings e

        bound result `shouldBe` M.fromList 
            [ (Name "id", T.length "\\x -> x")
            , (Name "id2", T.length "\\y -> y 162")
            ]

        args result `shouldBe` 
            [ Argument True (T.length "\\y -> y 162") -- id2
            , Argument False 3 -- 162
            , Argument False (T.length "(let id2 = \\y -> y 162 in") -- (let id2 ...)
            , Argument False 3 -- 433
            ]

    it "handles nested let without shadowing" $ do 
        {- 
           let outer = (let inner = 111 in
                        let inner2 = 2222 in 
                            1) in
               33
        -}
        let e = Let (Name "outer")
                    (Par (Let (Name "inner") (Lit (LNum 111))
                              (Let (Name "inner2") (Lit (LNum 2222))
                                   (Lit (LNum 1)))))
                    (Lit (LNum 22))

        let result = getBindings e

        bound result `shouldBe` M.fromList 
            [ (Name "outer", 21)
            , (Name "inner", T.length "111")
            , (Name "inner2", T.length "2222")
            ]

        args result `shouldBe` []

    it "handles nested let with shadowing" $ do 
        {- 
           let outer = (let inner = 111 in
                        let inner = 2222 in 
                            1) in
               33
        -}
        let e = Let (Name "outer")
                    (Par (Let (Name "inner") (Lit (LNum 111))
                              (Let (Name "inner") (Lit (LNum 2222))
                                   (Lit (LNum 1)))))
                    (Lit (LNum 22))

        let result = getBindings e

        bound result `shouldBe` M.fromList 
            [ (Name "outer", 20)
            , (Name "inner", T.length "2222")
            ]

        args result `shouldBe` []

    it "handles let bindings in applications" $ do 
        {- 
           (\x -> let foo = 1 in
                      1) x 
                                    (let bar = 22 in
                                         bar)
                                    1
                                    (let baz = 333 in
                                         baz)
        -}
        let e = App (Par (Lam [Name "x"] $
                               Let (Name "foo") (Lit (LNum 1)) $
                                   Lit (LNum 1)))
                    [ Var (Name "x")
                    , Par $ Let (Name "bar") (Lit (LNum 22)) $
                                 Var (Name "bar")
                    , Lit (LNum 1)
                    , Par $ Let (Name "baz") (Lit (LNum 333)) $
                                 Var (Name "baz")
                    ]

        let result = getBindings e

        bound result `shouldBe` M.fromList
            [ (Name "foo", 1)
            , (Name "bar", 2)
            , (Name "baz", 3)
            ]

        args result `shouldBe`
            [ Argument False 1 -- x
            , Argument False (T.length "(let bar = 22 in")
            , Argument False 1 -- 1
            , Argument False (T.length "(let baz = 333 in")
            ]
        

    it "traverses into lambdas" $ do 
        {-
           let outer = \foo -> let inner = 111 in
                               let inner2 = 2222 in 
                                   inner inner2 in
               f outer 
                 33
        -}
        let e = Let (Name "outer")
                    (Lam [Name "foo"] $
                         Let (Name "inner") (Lit (LNum 111)) $
                             Let (Name "inner2") (Lit (LNum 2222)) $
                                 App (Var (Name "inner"))
                                     [Var (Name "inner2")])
                    (App (Var (Name "f"))
                         [Var (Name "outer")
                         ,Lit (LNum 33)])
                    
        let result = getBindings e

        bound result `shouldBe` M.fromList
            [ (Name "outer", 28)
            , (Name "inner", T.length "111")
            , (Name "inner2", T.length "2222")
            ]

        args result `shouldBe` 
            [ Argument True (T.length "\\foo -> let inner2 = 2222 in")
            , Argument False 2 -- 33
            , Argument True (T.length "2222") -- inner2
            ]

    it "does the right thing at a given subtree?" $ do 
        pending

main :: IO ()
main = hspec spec
