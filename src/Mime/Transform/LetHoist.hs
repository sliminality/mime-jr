{-# LANGUAGE LambdaCase #-}

module Mime.Transform.LetHoist
    ( getArguments
    , LetState (..)
    , Argument (..)
    , getExamples
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Mime.Grammar (Expr (..), Lit (..), Name (..))

data Argument = Argument 
    { isBound :: Bool -- ^ Is the argument bound or inline?
    , expr :: Expr -- ^ Argument. If bound, this is the bound value.
    } deriving (Show, Eq)

data LetState = LetState 
    { bound :: Map Name Expr -- ^ Let-bindings in-scope at the expression.
    , args :: [Argument]    -- ^ Recursively accumulated arguments.
    }

emptyState :: LetState
emptyState = LetState M.empty []

-- | Adds a new let binding to the state.
addBinding :: Name -> Expr -> LetState -> LetState
addBinding x v (LetState bs as) = LetState bs' as
    where bs' = M.insert x v bs

-- | Adds a new argument used in an application.
addArgument :: LetState -> Expr -> [Argument]
addArgument (LetState bs as) e = arg:as
    where arg = mkArgument bs e

mkArgument :: Map Name Expr -> Expr -> Argument
mkArgument bs v@(Var x) = case M.lookup x bs of
   Just bv -> Argument True bv
   -- If it's not bound, compute the width of the identifier.
   Nothing -> Argument False v
mkArgument _ v = Argument False v

-- | Let-hoisting transformation.
--   Finds all of the arguments used in function applications,
--   and computes their formatted widths.
getArguments :: Expr -> LetState
getArguments = go emptyState
    where go :: LetState -> Expr -> LetState
          go m = \case
            Par e        -> go m e
            Lam _ body   -> go m body

            Let x v body -> go (addBinding x v m') body
                -- Recur into v, get arguments (but not bindings).
                where m' = LetState (bound m) (args $ go m v)

            App f vs     -> go m' f
                where m' = LetState (bound m) (args $ foldr traverseArg m vs)
                      traverseArg v acc = do
                        -- Capture any arguments, but not bindings.
                        let acc' = LetState (bound m) (addArgument acc v)
                        go acc' v -- Recur into argument.

            _         -> m

-- | Returns a list of input-output pairs, where the input is 
--   whether the argument is bound, and the output is the formatted
--   width of the argument.
getExamples :: Expr -> [(Bool, Expr)]
getExamples = map (\a -> (isBound a, expr a)) 
    . args 
    . getArguments 
