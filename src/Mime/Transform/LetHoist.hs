{-# LANGUAGE LambdaCase #-}

module Mime.Transform.LetHoist
    ( getBindings
    , LetState (..)
    , Argument (..)
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Mime.Document as Doc (IntoDoc (width))
import Mime.Grammar (Expr (..), Lit (..), Name (..))

data Argument = Argument 
    { isBound :: Bool -- ^ Is the argument bound or inline?
    , width :: Int    -- ^ Width of the formatted argument. If bound, use the width of the bound value.
    } deriving (Show, Eq)

data LetState = LetState 
    { bound :: Map Name Int -- ^ Accumulated let-bindings and bound value widths.
    , args :: [Argument] -- ^ Accumulated arguments.
    }

emptyState :: LetState
emptyState = LetState M.empty []

-- | Adds a new let binding to the state.
addBinding :: Name -> Expr -> LetState -> LetState
addBinding x v (LetState bs as) = LetState bs' as
    where bs' = M.insert x (Doc.width v) bs

-- | Adds a new argument used in an application.
addArgument :: LetState -> Expr -> LetState
addArgument (LetState bs as) e = LetState bs (arg:as)
    where arg = mkArgument bs e

mkArgument :: Map Name Int -> Expr -> Argument
mkArgument bs v@(Var x) = case M.lookup x bs of
   Just w -> Argument True w
   -- If it's not bound, compute the width of the identifier.
   Nothing -> Argument False (Doc.width v)
mkArgument _ v = Argument False (Doc.width v)

-- | Let-hoisting transformation.
--   Finds all of the arguments used in function applications,
--   and computes their formatted widths.
getBindings :: Expr -> LetState
getBindings = go emptyState
    where go :: LetState -> Expr -> LetState
          go m = \case
            Par e        -> go m e
            Lam _ body   -> go m body

            Let x v body -> go m'' body
                where m'  = go m v -- Recur into v, get bindings.
                      m'' = addBinding x v m'  -- Add x = v itself.

            App f vs     -> go (foldr traverseArg m vs) f
                where traverseArg v acc = do
                        let acc' = addArgument acc v
                        go acc' v -- Recur into argument.

            _         -> m


