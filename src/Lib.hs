{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Mime.Grammar
import Mime.Transform.LetHoist (getExamples)
import Mime.Synthesis (synth)

sample :: Expr
{- 
  let add = \x, y -> plus x y in
      foldr add 0 "hello"
 -}
sample = Let (Name "add")
             (Lam [Name "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", Name "y"] $
                  App (Var (Name "plus")) 
                      [Var (Name "x"), Var (Name "y")]) $
             App (Var (Name "foldr")) 
                 [ Var (Name "add")
                 , Lit (LNum 0)
                 , Lit (LStr "hello")
                 ]

someFunc :: IO ()
someFunc = do
    let xs = getExamples sample
    print xs

    case synth xs 100 of
      Just x -> print x
      _ -> print "No solution found"

