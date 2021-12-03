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
