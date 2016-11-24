{-|
 - Module      : Data.Lambda.Random.PlainSystem 
 - Description : Basic notions regarding combinatorial
 -               specifications defining plain lambda terms.
 - Copyright   : (c) Maciej Bendkowski, 2016
 - 
 - License     : BSD3
 - Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 - Stability   : experimental
 -}
module Data.Lambda.Random.PlainSystem
    ( -- * Boltzmann system utilities.
      PlainSystem(..)
    , boltzmannSystem

     -- * Boltzmann sampler utilities.
    , PlainSampler(..)
    , boltzmannSampler
    , rejectionSampler
    ) where

import Prelude hiding (abs)

import Data.Lambda ()
import Data.Lambda.Model
import Data.Lambda.Random.Oracle

-- | An expression defining the branching probabilities
--   in the Boltzmann model for plain lambda terms.
data PlainSystem a = PlainSystem { abs :: a   -- ^ Abstraction probability.
                                 , app :: a   -- ^ Application probability.
                                 , zero :: a  -- ^ Zero constructor probability.
                                 } deriving (Show)

eval :: (Floating a, Integral b) => Model b -> a -> a
eval m z = -1/(2*z^^d) * (z^^c - 1 + sqrt ((4*z^^(a+d))/(z^^b - 1) + (z^^c - 1)^^2))
    where (a,b,c,d) = weights m

evalD :: (Floating a, Integral b) => Model b -> a -> a
evalD m z = (z^^a)/(1-z^^b)
    where (a,b,_,_) = weights m

-- | Computes the Boltzmann model for plain 
--   lambda terms in the given parameter.
boltzmannSystem :: (Floating a, Integral b) 
                => Model b           -- ^ Size notion. 
                -> a                 -- ^ Formal z parameter.
                -> PlainSystem a     -- ^ The computed Boltzmann system.

boltzmannSystem m z = PlainSystem { abs = z^^c
                                  , app = z^^c + z^^d * eval m z
                                  , zero = z^^a / evalD m z
                                  }
    where (a,_,c,d) = weights m

-- | Boltzmann sampler specification consisting of a Boltzmann system
--   with a corresponding and consistent size notion model.
data PlainSampler a b = PlainSampler { system :: PlainSystem a     -- ^ Boltzmann system.
                                     , model :: Model b            -- ^ Size notion.
                                     }

-- | Computes the Boltzmann sampler specification 
--   for plain lambda terms in the given parameter.
boltzmannSampler :: (Floating a, Integral b)
                 => Model b              -- ^ Size notion. 
                 -> a                    -- ^ Formal z parameter.
                 -> PlainSampler a b     -- ^ The computed Boltzmann sampler.

boltzmannSampler m z = let sys = boltzmannSystem m z in
                           PlainSampler { system = sys
                                        , model = m
                                        }

-- | Computes the rejection Boltzmann sampler for plain
--   lambda terms evaluated near the dominating singularity.
rejectionSampler :: (Floating a, Ord a, Integral b)
                 => Model b              -- ^ Size notion. 
                 -> a                    -- ^ Singularity approximation error.
                 -> PlainSampler a b     -- ^ The computed rejection Boltzmann sampler.
                
rejectionSampler m eps = boltzmannSampler m rho
    where rho = domSing m eps
