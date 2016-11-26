{-|
 Module      : Data.Lambda.Random 
 Description : Boltzmann samplers for random lambda terms.
 Copyright   : (c) Maciej Bendkowski, 2016

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Boltzmann samplers for plain and closed h-shallow lambda terms (closed lambda terms
 in which each de Bruijn index is bounded by h). 
     
 The exact outcome size of a Boltzmann sampler is a random variable. Its moments, in 
 particular expectation, can be calibrated by adjusting the formal
 evaluation parameter (see "Data.Lambda.Random.System" or "Data.Lambda.Random.PlainSystem").
 Boltzmann samplers guarantee that terms of equal size have equal probability of being chosen. 
-}
module Data.Lambda.Random
    ( -- * Closed h-shallow lambda terms
      closedLambda
    , closedLambdaIO

      -- * Filter samplers for closed h-shallow lambda terms
    , filterClosed
    , filterClosedIO
      
      -- * Plain lambda terms
    , plainLambda
    , plainLambdaIO
      
      -- * Filter samplers for plain lambda terms
    , filterPlain
    , filterPlainIO
    ) where

import Prelude hiding (abs)

import Control.Monad
import Control.Monad.Random

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Lambda
import Data.Lambda.Model

import qualified Data.Lambda.Random.System as S
import qualified Data.Lambda.Random.PlainSystem as P

randomP :: (Random a, Num a, RandomGen g) => MaybeT (Rand g) a 
randomP = lift $ getRandomR (0,1)

pass :: S.System a -> S.System a
pass [e] = [e]
pass (_:es) = es
pass _ = error "Can't pass an empty system!"

randomClosedLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
                   => S.Sampler a b
                   -> b
                   -> MaybeT (Rand g) (Lambda, b)

randomClosedLambda spec = randomClosedLambda' m sys
    where (m,sys) = (S.model spec, S.system spec)

randomClosedLambda' :: (Random a, Num a, Ord a, Integral b, RandomGen g) 
                    => Model b -> [S.Expr a] -> b -> MaybeT (Rand g) (Lambda, b)

randomClosedLambda' m sys @ (e:_) ub = do
    guard (ub > 0)
    p <- randomP
    if p < S.abs e then do
        let w = absW m 
        (x,s) <- randomClosedLambda' m (pass sys) (ub - w)
        return (Abs x, s + w)
    else if p < S.app e then do
        let w = appW m
        (x,s) <- randomClosedLambda' m sys (ub - w)
        (x',s') <- randomClosedLambda' m sys (ub - w - s)
        return (App x x', s + s' + w)
    else do
        (x,s) <- randomIndex m ub p (S.idx e)
        return (Var x, s)

randomClosedLambda' _ _ _ = error "I wasn't expecting the Spanish Inquisition."

randomIndex :: (Random a, Num a, Ord a, Integral b, RandomGen g)
            => Model b
            -> b
            -> a
            -> [a] 
            -> MaybeT (Rand g) (Index, b)

randomIndex m ub = randomIndex' m (ub - w) Z w
    where w = zeroW m

randomIndex' :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => Model b -> b -> Index -> b -> a -> [a] -> MaybeT (Rand g) (Index, b)

randomIndex' m ub idx w p ps = do
    guard (ub > 0)
    case ps of
      (p':ps') -> 
          if p < p' then return (idx, w)
                    else do
                      let w' = succW m
                      randomIndex' m (ub - w') (S idx) (w + w') p ps'
      _ -> return (idx, w)

randomLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => P.PlainSampler a b -> b -> MaybeT (Rand g) (Lambda, b)

randomLambda spec = randomLambda' m sys
    where (m,sys) = (P.model spec, P.system spec)

randomLambda' :: (Random a, Num a, Ord a, Integral b, RandomGen g)
              =>  Model b -> P.PlainSystem a -> b -> MaybeT (Rand g) (Lambda, b)

randomLambda' m sys ub = do
    guard (ub > 0)
    p <- randomP
    if p < P.abs sys then do
        let w = absW m
        (x,s) <- randomLambda' m sys (ub - w)
        return (Abs x, s + w)
    else if p < P.app sys then do
        let w = appW m
        (x,s) <- randomLambda' m sys (ub - w)
        (x',s') <- randomLambda' m sys (ub - w - s)
        return (App x x', s + s' + w)
    else do
        (x,s) <- randomPlainIndex m ub (P.zero sys)
        return (Var x, s)

randomPlainIndex :: (Random a, Num a, Ord a, Integral b, RandomGen g)
                 =>  Model b -> b -> a -> MaybeT (Rand g) (Index, b)

randomPlainIndex m ub zeroP = do
    guard (ub > 0)
    p <- randomP
    if p < zeroP then return (Z, zeroW m)
    else do
        let w = succW m
        (idx, s) <- randomPlainIndex m (ub - w) zeroP
        return (S idx, s + w)
    
-- | Samples a random closed h-shallow lambda term in the given size range.
closedLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => S.Sampler a b     -- ^ Boltzmann sampler to use.
             -> b                 -- ^ Outcome size lower bound.
             -> b                 -- ^ Outcome size upper bound.
             -> Rand g Lambda     -- ^ The monadic result. 

closedLambda spec lb ub = do
    sample <- runMaybeT $ randomClosedLambda spec ub
    case sample of
      Nothing -> closedLambda spec lb ub
      Just (t, w) -> if lb <= w then return t 
                                else closedLambda spec lb ub

-- | Samples a random closed h-shallow lambda term in the given size range
--   using the IO monad. See `closedLambda' for more details.
closedLambdaIO :: (Random a, Num a, Ord a, Integral b) 
               => S.Sampler a b     -- ^ Boltzmann sampler to use.
               -> b                 -- ^ Outcome size lower bound.
               -> b                 -- ^ Outcome size upper bound.
               -> IO Lambda         -- ^ The monadic result. 

closedLambdaIO spec lb ub = evalRandIO rand
    where rand = closedLambda spec lb ub

-- | Samples a random plain lambda term in the given size range.
plainLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
            => P.PlainSampler a b   -- ^ Boltzmann sampler to use.
            -> b                    -- ^ Outcome size lower bound.
            -> b                    -- ^ Outcome size upper bound.
            -> Rand g Lambda        -- ^ The monadic result.

plainLambda spec lb ub = do
    sample <- runMaybeT $ randomLambda spec ub
    case sample of
      Nothing -> plainLambda spec lb ub
      Just (t, w) -> if lb <= w then return t
                                else plainLambda spec lb ub

-- | Samples a random plain lambda term in the given size range
--   using the IO monad. See `plainLambda' for more details.
plainLambdaIO :: (Random a, Num a, Ord a, Integral b) 
              => P.PlainSampler a b     -- ^ Boltzmann sampler to use.
              -> b                      -- ^ Outcome size lower bound.
              -> b                      -- ^ Outcome size upper bound.
              -> IO Lambda              -- ^ The monadic result. 

plainLambdaIO spec lb ub = evalRandIO rand
    where rand = plainLambda spec lb ub

-- | Samples a random closed h-shallow lambda term in the given
--   size range. In addition, the term has to satisfy the given predicate.
--   See also `closedLambda'.
filterClosed :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => (Lambda -> Bool)  -- ^ Filter function to use.
             -> S.Sampler a b     -- ^ Boltzmann sampler to use.
             -> b                 -- ^ Outcome size lower bound.
             -> b                 -- ^ Outcome size upper bound.
             -> Rand g Lambda     -- ^ The monadic result. 
       
filterClosed p spec lb ub = do
    t <- closedLambda spec lb ub
    if p t then return t
           else filterClosed p spec lb ub

-- | Samples a random closed h-shallow lambda term in the given
--   size range. In addition, the term has to satisfy the given predicate.
--   The IO monad is used as the source of random numbers.
--   See also `filterClosed'.
filterClosedIO :: (Random a, Num a, Ord a, Integral b)
               => (Lambda -> Bool)  -- ^ Filter function to use.
               -> S.Sampler a b     -- ^ Boltzmann sampler to use.
               -> b                 -- ^ Outcome size lower bound.
               -> b                 -- ^ Outcome size upper bound.
               -> IO Lambda         -- ^ The monadic result. 

filterClosedIO p spec lb ub = evalRandIO rand
    where rand = filterClosed p spec lb ub

-- | Samples a random plain lambda term in the given
--   size range. In addition, the term has to satisfy the given predicate.
--   See also `plainLambda'.
filterPlain :: (Random a, Num a, Ord a, Integral b, RandomGen g)
            => (Lambda -> Bool)       -- ^ Filter function to use.
            -> P.PlainSampler a b     -- ^ Boltzmann sampler to use.
            -> b                      -- ^ Outcome size lower bound.
            -> b                      -- ^ Outcome size upper bound.
            -> Rand g Lambda          -- ^ The monadic result. 
       
filterPlain p spec lb ub = do
    t <- plainLambda spec lb ub
    if p t then return t
           else filterPlain p spec lb ub

-- | Samples a random plain lambda term in the given
--   size range. In addition, the term has to satisfy the given predicate.
--   The IO monad is used as the source of random numbers.
--   See also `filterPlain'.
filterPlainIO :: (Random a, Num a, Ord a, Integral b)
              => (Lambda -> Bool)       -- ^ Filter function to use.
              -> P.PlainSampler a b     -- ^ Boltzmann sampler to use.
              -> b                      -- ^ Outcome size lower bound.
              -> b                      -- ^ Outcome size upper bound.
              -> IO Lambda              -- ^ The monadic result. 

filterPlainIO p spec lb ub = evalRandIO rand
    where rand = filterPlain p spec lb ub
