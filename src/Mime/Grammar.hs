{-# LANGUAGE OverloadedStrings #-}

-- Define a simple grammr for a toy language.

module Mime.Grammar (Expr (..), Lit (..), repr) where

import Data.Text (Text)
import qualified Data.Text as T
import Mime.Document

type Name = Text

data Expr = Var Name             -- x
          | Let Name Expr Expr   -- let x = e1 in e2
          | Lam Name Expr        -- \x -> e
          | App Expr Expr        -- e1 e2
          | Lit Lit              -- "hello", True, 0, etc.

data Lit = LStr Text     -- "hello"
         | LNum Int      -- 0, 1, 2, ...

programIndent :: Int
programIndent = 4

-- | Represent a single expression.
repr :: Expr -> Text
repr (Var x) = text x
repr (Let x v b) = text "let "
    <.> text x
    <.> text " = " 
    <.> repr v 
    <.> text " in" 
    <.> nest programIndent (line <.> repr b)
repr (Lam x b) = text "\\" 
    <.> text x
    <.> text " -> " 
    <.> repr b
repr (App f v) = repr f 
    <.> text " " 
    <.> repr v 
repr (Lit (LStr s)) = text s
repr (Lit (LNum n)) = text $ T.pack $ show n

