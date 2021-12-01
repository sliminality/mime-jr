{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Mime.Grammar
import Mime.Transform.LetHoist (getExamples)

sample :: Expr
{- 
  let add = \x, y -> plus x y in
      foldr add 0 "hello"
 -}
sample = Let (Name "add")
             (Lam [Name "x", Name "y"] $
                  App (Var (Name "plus")) 
                      [Var (Name "x"), Var (Name "y")]) $
             App (Var (Name "foldr")) 
                 [ Var (Name "add")
                 , Lit (LNum 0)
                 , Lit (LStr "hello")
                 ]

someFunc :: IO ()
someFunc = do
    let exs = getExamples sample
    print exs
