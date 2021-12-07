{-# LANGUAGE FunctionalDependencies #-}

module Mime.Guard 
    ( N (..)
    , B (..)
    , Denote (..)
    ) where

import Mime.Document (IntoDoc (width))
import Mime.Grammar

data N = NWidth | NConst Int

data B = BTrue
       | BFalse
       | BGt  N N
       | BEq  N N
       | BAnd B B
       | BOr  B B
       | BNot B

class Denote a b | a -> b where
    denote :: a -> Expr -> b

instance Denote N Int where
    denote NWidth = width
    denote (NConst n) = const n

instance Denote B Bool where
    denote BTrue        _ = True
    denote BFalse       _ = False
    denote (BGt  n1 n2) e = denote n1 e >  denote n2 e
    denote (BEq  n1 n2) e = denote n1 e == denote n2 e
    denote (BAnd b1 b2) e = denote b1 e && denote b2 e
    denote (BOr  b1 b2) e = denote b1 e || denote b2 e
    denote (BNot b)     e = not $ denote b e

instance Show N where
    show NWidth = "width"
    show (NConst x) = show x

instance Show B where
    show BTrue = "⊤"
    show BFalse = "⊥"
    show (n1 `BGt` n2) = show n1 ++ " > " ++ show n2
    show (n1 `BEq` n2) = show n1 ++ " = " ++ show n2
    show (b1 `BAnd` b2) = show b1 ++ " && " ++ show b2
    show (b1 `BOr` b2) = show b1 ++ " || " ++ show b2
    show (BNot b) = "!" ++ show b
