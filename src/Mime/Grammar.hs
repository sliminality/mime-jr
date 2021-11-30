{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- Define a simple grammr for a toy language.

module Mime.Grammar (Expr (..), Lit (..), repr) where

import Data.Text (Text)
import qualified Data.Text as T
import Mime.Document (Doc (..), IntoDoc (..))

type Name = Text

data Expr = Var Name             -- x
          | Let Name Expr Expr   -- let x = e1 in e2
          | Lam [Name] Expr      -- \x1, x2 -> e
          | App Expr [Expr]      -- e1 e2 e3
          | Lit Lit              -- "hello", True, 0, etc.
          | Par Expr             -- (e)

data Lit = LStr Text             -- "hello"
         | LNum Int              -- 0, 1, 2, ...

indentWidth :: Int
indentWidth = 4

instance IntoDoc Expr where
    repr :: (Doc d) => Expr -> d
    repr (Var x) = text x

    repr (Let x v b) = text "let "
        <.> text x
        <.> text " = " 
        <.> repr v 
        <.> text " in" 
        <.> case b of 
              -- Don't indent a subsequent `let` binding.
              Let {} -> line <.> repr b 
              _      -> nest indentWidth (line <.> repr b)

    repr (Lam xs b) = text "\\" 
        <.> args
        <.> text " -> " 
        <.> repr b 
            where args = text (T.intercalate ", " xs)

    repr (App f vs) = fname
        <.> text " "
        <.> nest (1 + T.length fname) (reprArgs vs)
            where fname :: (Doc d) => d
                  fname = repr f

                  reprArgs :: (Doc d) => [Expr] -> d
                  reprArgs [] = nil
                  reprArgs [v'] = repr v'
                  reprArgs (v':vs') = repr v'
                      <.> line
                      <.> reprArgs vs'

    repr (Lit (LStr s)) = text s
    repr (Lit (LNum n)) = text $ T.pack $ show n
    repr (Par e) = text "("
        <.> nest 1 (repr e)
        <.> text ")"
