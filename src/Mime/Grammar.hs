{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Define a simple grammr for a toy language.

module Mime.Grammar (Expr (..), Lit (..), Name (..)) where

import Data.Text (Text)
import qualified Data.Text as T
import Mime.Document (Doc (..), IntoDoc (..))

newtype Name = Name { unName :: Text } 
    deriving (Eq, Show, Ord)

data Expr = Var Name             -- x
          | Let Name Expr Expr   -- let x = e1 in e2
          | Lam [Name] Expr      -- \x1, x2 -> e
          | App Expr [Expr]      -- e1 e2 e3
          | Lit Lit              -- "hello", True, 0, etc.
          | Par Expr             -- (e)

data Lit = LStr Text             -- "hello"
         | LNum Int              -- 0, 1, 2, ...

instance Show Expr where
    show = T.unpack . repr

indentWidth :: Int
indentWidth = 4

instance IntoDoc Expr where
    -- | Computes the width of a formatted expression, possibly over multiple lines.
    width :: Expr -> Int
    width = maximum . map T.length . T.lines . repr

    -- | Format an expression as a Document.
    repr :: forall d. (Doc d) => Expr -> d
    repr (Var (Name x)) = text x

    repr (Let (Name x) v b) = 
        let decl = text "let "
                   <.> text x
                   <.> text " = " in 
            decl 
        <.> nest (len decl) (repr v) 
        <.> text " in" 
        <.> case b of 
              -- Don't indent a subsequent `let` binding.
              Let {} -> line <.> repr b 
              _      -> nest indentWidth (line <.> repr b)

    repr (Lam xs b) = text "\\" 
        <.> text args
        <.> text " -> " 
        <.> repr b 
            where args = T.intercalate ", " $ map unName xs

    repr (App f vs) = fname
        <.> text " "
        <.> nest (1 + len fname) (reprArgs vs)
            where fname :: d
                  fname = repr f

                  reprArgs :: [Expr] -> d
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
