{-|
 - Module      : Data.Lambda.Random 
 - Description : Boltzmann samplers for random lambda terms.
 - Copyright   : (c) Maciej Bendkowski, 2016
 -
 - License     : BSD3
 - Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 - Stability   : experimental
 -}
module Data.Lambda.Random
    ( -- * Boltzmann samplers for closed h-shallow lambda terms.
      closedLambda
    , closedLambdaIO

      -- * Filter samplers for closed h-shallow lambda terms.
    , filterClosed
    , filterClosedIO
      
      -- * Boltzmann samplers for plain lambda terms.
    , plainLambda
    , plainLambdaIO
      
      -- * Filter samplers for plain lambda terms.
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
pass (e:es) = es

randomClosedLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
                   => S.Sampler a b
                   -> b
                   -> MaybeT (Rand g) (Lambda, b)

randomClosedLambda spec = randomClosedLambda' m sys
    where (m,sys) = (S.model spec, S.system spec)

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

randomIndex :: (Random a, Num a, Ord a, Integral b, RandomGen g)
            => Model b
            -> b
            -> a
            -> [a] 
            -> MaybeT (Rand g) (Index, b)

randomIndex m ub = randomIndex' m (ub - w) Z w
    where w = zeroW m

randomIndex' m ub idx w p ps = do
    guard (ub > 0)
    case ps of
      (p':ps') -> 
          if p < p' then return (idx, w)
                    else do
                      let w' = succW m
                      randomIndex' m (ub - w') (S idx) (w + w') p ps'
      _ -> return (idx, w)

randomLambda spec = randomLambda' m sys
    where (m,sys) = (P.model spec, P.system spec)

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

randomPlainIndex m ub zeroP = do
    guard (ub > 0)
    p <- randomP
    if p < zeroP then return (Z, zeroW m)
    else do
        let w = succW m
        (idx, s) <- randomPlainIndex m (ub - w) zeroP
        return (S idx, s + w)
    
-- | Samples a random closed h-shallow lambda term in the given size range.
--   The exact outcome size is a random variable itself, however any
--   two lambda terms of the same size have the same probability
--   due to the imposed Boltzmann model.
closedLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => S.Sampler a b     -- ^ Boltzmann sampler to use.
             -> b                 -- ^ Outcome lower bound.
             -> b                 -- ^ Outcome upper bound.
             -> Rand g Lambda     -- ^ The monadic result. 

closedLambda spec lb ub = do
    sample <- runMaybeT $ randomClosedLambda spec ub
    case sample of
      Nothing -> closedLambda spec lb ub
      Just (t, w) -> if lb <= w then return t 
                                else closedLambda spec lb ub

-- | Samples a random closed h-shallow lambda term in the given size range
--   using the IO monad as the source of its random generator.
--   See closedLambda for more details.
closedLambdaIO :: (Random a, Num a, Ord a, Integral b) 
               => S.Sampler a b     -- ^ Boltzmann sampler to use.
               -> b                 -- ^ Outcome lower bound.
               -> b                 -- ^ Outcome upper bound.
               -> IO Lambda         -- ^ The monadic result. 

closedLambdaIO spec lb ub = evalRandIO rand
    where rand = closedLambda spec lb ub

-- | Samples a random plain lambda term in the given size range.
--   The exact outcome size is a random variable itself, however any
--   two lambda terms of the same size have the same probability
--   due to the imposed Boltzmann model.
plainLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
            => P.PlainSampler a b   -- ^ Boltzmann sampler to use.
            -> b                    -- ^ Outcome lower bound.
            -> b                    -- ^ Outcome upper bound.
            -> Rand g Lambda        -- ^ The monadic result.

plainLambda spec lb ub = do
    sample <- runMaybeT $ randomLambda spec ub
    case sample of
      Nothing -> plainLambda spec lb ub
      Just (t, w) -> if lb <= w then return t
                                else plainLambda spec lb ub

-- | Samples a random plain lambda term in the given size range
--   using the IO monad as the source of its random generator.
--   See plainLambda for more details.
plainLambdaIO :: (Random a, Num a, Ord a, Integral b) 
              => P.PlainSampler a b     -- ^ Boltzmann sampler to use.
              -> b                      -- ^ Outcome lower bound.
              -> b                      -- ^ Outcome upper bound.
              -> IO Lambda              -- ^ The monadic result. 

plainLambdaIO spec lb ub = evalRandIO rand
    where rand = plainLambda spec lb ub

-- | Samples a random closed h-shallow lambda term in the given
--   size range, which satisfies the given predicate.
--   See also closedLambda.
filterClosed :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => (Lambda -> Bool)  -- ^ Filter function to use.
             -> S.Sampler a b     -- ^ Boltzmann sampler to use.
             -> b                 -- ^ Outcome lower bound.
             -> b                 -- ^ Outcome upper bound.
             -> Rand g Lambda     -- ^ The monadic result. 
       
filterClosed p spec lb ub = do
    t <- closedLambda spec lb ub
    if p t then return t
           else filterClosed p spec lb ub

-- | Samples a random closed h-shallow lambda term in the given
--   size range, which satisfies the given predicate. The IO monad 
--   is used as the source of its random generator.
--   See also filter.
filterClosedIO :: (Random a, Num a, Ord a, Integral b)
               => (Lambda -> Bool)  -- ^ Filter function to use.
               -> S.Sampler a b     -- ^ Boltzmann sampler to use.
               -> b                 -- ^ Outcome lower bound.
               -> b                 -- ^ Outcome upper bound.
               -> IO Lambda         -- ^ The monadic result. 

filterClosedIO p spec lb ub = evalRandIO rand
    where rand = filterClosed p spec lb ub

-- | Samples a random plain lambda term in the given
--   size range, which satisfies the given predicate.
--   See also plainLambda.
filterPlain :: (Random a, Num a, Ord a, Integral b, RandomGen g)
            => (Lambda -> Bool)       -- ^ Filter function to use.
            -> P.PlainSampler a b     -- ^ Boltzmann sampler to use.
            -> b                      -- ^ Outcome lower bound.
            -> b                      -- ^ Outcome upper bound.
            -> Rand g Lambda          -- ^ The monadic result. 
       
filterPlain p spec lb ub = do
    t <- plainLambda spec lb ub
    if p t then return t
           else filterPlain p spec lb ub

-- | Samples a random plain lambda term in the given
--   size range, which satisfies the given predicate. The IO monad 
--   is used as the source of its random generator.
--   See also filterPlain.
filterPlainIO :: (Random a, Num a, Ord a, Integral b)
              => (Lambda -> Bool)       -- ^ Filter function to use.
              -> P.PlainSampler a b     -- ^ Boltzmann sampler to use.
              -> b                      -- ^ Outcome lower bound.
              -> b                      -- ^ Outcome upper bound.
              -> IO Lambda              -- ^ The monadic result. 

filterPlainIO p spec lb ub = evalRandIO rand
    where rand = filterPlain p spec lb ub
