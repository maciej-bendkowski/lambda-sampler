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
    ( -- * Boltzmann sampler for closed h-shallow lambda terms.
      closedLambda
    , closedLambdaIO

      -- * Filter samplers.
    , filter
    , filterIO
    ) where

import Prelude hiding (abs, filter)

import Control.Monad
import Control.Monad.Random

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Lambda
import Data.Lambda.Model
import Data.Lambda.Random.System

randomP :: (Random a, Num a, RandomGen g) => MaybeT (Rand g) a 
randomP = lift $ getRandomR (0,1)

pass :: System a -> System a
pass [e] = [e]
pass (e:es) = es

randomClosedLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
                   => Sampler a b
                   -> b
                   -> MaybeT (Rand g) (Lambda, b)

randomClosedLambda spec = randomClosedLambda' m sys
    where (m,sys) = (model spec, system spec)

randomClosedLambda' m sys @ (e:_) ub = do
    guard (ub > 0)
    p <- randomP
    if p < abs e then do
        let w = absW m 
        (x,s) <- randomClosedLambda' m (pass sys) (ub - w)
        return (Abs x, s + w)
    else if p < app e then do
        let w = appW m
        (x,s) <- randomClosedLambda' m sys (ub - w)
        (x',s') <- randomClosedLambda' m sys (ub - w - s)
        return (App x x', s + s' + w)
    else do
        (x,s) <- randomIndex m ub p (idx e)
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

-- | Samples a random closed h-shallow lambda term in the given size range.
--   The exact outcome size is a random variable itself, however any
--   two lambda terms of the same size have the same probability
--   due to the imposed Boltzmann model.
closedLambda :: (Random a, Num a, Ord a, Integral b, RandomGen g)
             => Sampler a b     -- ^ Boltzmann sampler to use.
             -> b               -- ^ Outcome lower bound.
             -> b               -- ^ Outcome upper bound.
             -> Rand g Lambda   -- ^ The monadic result. 

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
               => Sampler a b     -- ^ Boltzmann sampler to use.
               -> b               -- ^ Outcome lower bound.
               -> b               -- ^ Outcome upper bound.
               -> IO Lambda       -- ^ The monadic result. 

closedLambdaIO spec lb ub = evalRandIO rand
    where rand = closedLambda spec lb ub

-- | Samples a random closed h-shallow lambda term in the given
--   size range, which satisfies the given predicate.
--   See also closedLambda.
filter :: (Random a, Num a, Ord a, Integral b, RandomGen g)
       => (Lambda -> Bool)  -- ^ Filter function to use.
       -> Sampler a b       -- ^ Boltzmann sampler to use.
       -> b                 -- ^ Outcome lower bound.
       -> b                 -- ^ Outcome upper bound.
       -> Rand g Lambda     -- ^ The monadic result. 
       
filter p spec lb ub = do
    t <- closedLambda spec lb ub
    if p t then return t
           else filter p spec lb ub

-- | Samples a random closed h-shallow lambda term in the given
--   size range, which satisfies the given predicate. The IO monad 
--   is used as the source of its random generator.
--   See also filter.
filterIO :: (Random a, Num a, Ord a, Integral b)
         => (Lambda -> Bool)  -- ^ Filter function to use.
         -> Sampler a b       -- ^ Boltzmann sampler to use.
         -> b                 -- ^ Outcome lower bound.
         -> b                 -- ^ Outcome upper bound.
         -> IO Lambda         -- ^ The monadic result. 

filterIO p spec lb ub = evalRandIO rand
    where rand = filter p spec lb ub
