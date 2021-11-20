{-# LANGUAGE OverloadedStrings #-}

-- Define a simple grammr for a toy language.

module Mime.Grammar (Expr (..), Lit (..), repr) where

import Data.Text as T

type Name = Text

data Expr = Var Name             -- x
          | Let Name Expr Expr   -- let x = e1 in e2
          | Lam Name Expr        -- \x -> e
          | App Expr Expr        -- e1 e2
          | Lit Lit              -- "hello", True, 0, etc.

data Lit = LStr Text     -- "hello"
         | LNum Int      -- 0, 1, 2, ...

-- | Prints the concrete syntax of an expression.
repr :: Expr -> Text
repr (Var x) = x
repr (Let x v b) = "let " <> x <> " = " <> repr v <> " in\n" <> repr b
repr (Lam x b) = "λ" <> x <> "." <> repr b
repr (App f v) = repr f <> " " <> repr v
repr (Lit (LStr s)) = s
repr (Lit (LNum n)) = T.pack $ show n


