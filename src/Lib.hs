{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Bifunctor (second)
import Mime.Document (IntoDoc (width))
import Mime.Grammar
import Mime.Transform.LetHoist (getExamples)
import Mime.Synthesis (synth)

sample :: Expr
{- 
  let myList = "abcdefghijklmnopqrstuvwxyz" in
  let add = \x, y -> plus x y in
      foldr add 0 myList
 -}

sample = Let (Name "myList")
             (Lit (LStr "abcdefghijklmnopqrstuvwxyz")) $
         Let (Name "add")
             (Lam [Name "x", Name "y"] $
                  App (Var (Name "plus")) 
                      [Var (Name "x"), Var (Name "y")]) $
             App (Var (Name "foldr")) 
                 [ Var (Name "add")
                 , Lit (LNum 0)
                 , Var (Name "myList")
                 ]

sampleDisjoint = Let (Name "init")
                     (Lit (LNum 0)) $
                 Let (Name "add")
                     (Lam [Name "x", Name "y"] $
                          App (Var (Name "plus")) 
                              []) $
                     App (Var (Name "foldr")) 
                         [ Var (Name "add")
                         , Var (Name "init")
                         -- , Lit (LNum 0)
                         , Lit (LStr "hello")
                         ]

someFunc :: IO ()
someFunc = do
    putStrLn "\n******** Synthesizing bounds for program:"
    print sampleDisjoint

    putStrLn "\n******** Extracting examples:"
    let xs = getExamples sampleDisjoint
    print $ second width <$> xs

    putStrLn "\n******** Synthesized bound:"
    res <- synth xs 3
    case res of
      Just x -> print x
      _ -> print "No solution found"

