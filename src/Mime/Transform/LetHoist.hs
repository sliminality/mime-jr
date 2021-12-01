{-# LANGUAGE LambdaCase #-}

module Mime.Transform.LetHoist
    ( getBindings
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Mime.Document (IntoDoc (width))
import Mime.Grammar (Expr (..), Lit (..), Name (..))

-- | Let-hoisting transformation.
-- | Finds all of the arguments used in function applications,
-- | and computes their formatted widths.
getBindings :: Expr -> Map Name Int
getBindings = go M.empty
    where go :: Map Name Int -> Expr -> Map Name Int
          go m = \case
            Let x v b -> go m'' b
                where m'  = go m v -- Recur into v, get bindings.
                      m'' = M.insert x (width v) m'  -- Add x = v itself.
            Lam _ b   -> go m b
            App f vs  -> go m' f
                where m' = foldr (flip go) m vs -- Traverse the arguments first.
            Par e     -> go m e
            _         -> m
