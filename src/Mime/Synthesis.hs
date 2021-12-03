{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mime.Synthesis 
    ( synth
    ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Mime.Grammar
import Mime.Guard

-- Listing all the terms by arity until we find a better abstraction.
n0 :: [N]
n0 = [ NWidth
     , NConst 0
     , NConst 10
     , NConst 20
     , NConst 40
     , NConst 80
     , NConst 100
     , NConst 120
     ]

n1 :: [Int -> N]
n1 = [NConst]

b0 :: [B]
b0 = [BTrue, BFalse]

b1 :: [B -> B]
b1 = [BNot]

b2n :: [N -> N -> B]
b2n = [BGt, BEq]

b2b :: [B -> B -> B]
b2b = [BAnd, BOr]
    
data Programs = Programs { pn :: [N] , pb :: [B] }

-- | Synthesize a guard that matches the given input-output examples.
-- Thank you Armando Solar-Lezama for this blessing.
-- https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm
synth :: [(Bool, Expr)] -> Int -> Maybe B
synth xs iters = go iters (Programs n0 b0) 
    where go :: Int -> Programs -> Maybe B
          go iters ps = if iters == 0 then Nothing else do
              let ins = map snd xs 
              let ps' = prune ins $ grow ps
              case find (correct xs) (pb ps') of
                Nothing -> go (iters - 1) ps'
                g -> g

-- | Prune terms that are observationally equivalent.
prune :: [Expr] -> Programs -> Programs
prune ins (Programs pns pbs) = do
    -- Store program outputs in a map.
    -- Allow at most one program per output value.
    let pns' = foldr (eval ins) M.empty pns
    let pbs' = foldr (eval ins) M.empty pbs
    Programs (M.elems pns') (M.elems pbs')

-- | Evaluate the guard on the expression.
-- eval :: [Expr] -> _ -> Map Expr _ -> Map Expr _
eval ins g seen = 
    let outs = map (g `denote`) ins in
        if M.member outs seen
            -- If we've already seen this output, we'll omit it.
            then seen
            else M.insert outs g seen

-- | Use non-terminals to construct new terms from all terms in the list.
grow :: Programs -> Programs
grow ps = Programs (growN ps) (growB ps)

-- TODO: Figure out how to generate new constants.
growN :: Programs -> [N]
growN = pn

growB :: Programs -> [B]
growB ps = do
    ps1 <- growB1  ps <$> b1
    ps2 <- growB2n ps <$> b2n
    ps3 <- growB2b ps <$> b2b
    ps1 ++ ps2 ++ ps3

growB2n :: Programs -> (N -> N -> B) -> [B]
growB2n (Programs pns _) f = do
    -- Compute all pairs of numeric inputs.
    let pairs = [(n1, n2) | n1 <- pns, n2 <- pns]
    -- Apply the new term to every existing term.
    map (uncurry f) pairs

growB2b :: Programs -> (B -> B -> B) -> [B]
growB2b (Programs _ pbs) f = do
    -- Doing this a different way because I got bored.
    b1 <- pbs
    f b1 <$> pbs

growB1 :: Programs -> (B -> B) -> [B]
growB1 (Programs _ pbs) f = map f pbs

-- | Checks if a synthesized guard is correct wrt the examples.
correct :: [(Bool, Expr)] -> B -> Bool
correct xs g = all (pred g) xs 
    where pred g (out, e) = g `denote` e == out
