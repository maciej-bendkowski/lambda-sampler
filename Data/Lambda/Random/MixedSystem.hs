{-|
 Module      : Data.Lambda.Random.MixedSystem
 Description : Basic notions regarding combinatorial systems
               defining lambda terms.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Combinatorial system defining closed lambda terms in the de Bruijn notation.
-}
module Data.Lambda.Random.MixedSystem
    ( -- * System
      MixedSystem(..)
    , boltzmannSystem

    -- * Boltzmann samplers
    , MixedSampler(..)
    , boltzmannSampler
    , rejectionSampler
    ) where

import Prelude hiding (abs)

import Data.Lambda ()
import Data.Lambda.Model
import Data.Lambda.Random.Oracle

import qualified Data.Lambda.Random.System as S
import qualified Data.Lambda.Random.PlainSystem as P

-- | An expression defining the branching probabilities
--   in the Boltzmann model for unbounded closed lambda terms using
--   a mixture of a system for closed shallow terms and a closure
--   system for plain terms used once the closed system has been
--   exceeded in the sampling procedure.
data MixedSystem a = MixedSystem { shallowSystem :: S.System a
                                 , plainSystem :: P.PlainSystem a
                                 }

-- | Computes the Boltzmann model for closed lambda terms
--   evaluated in the given parameter using closed h-shallow
--   terms as a base approximation.
boltzmannSystem :: (Floating a, Integral b)
                => Model b        -- ^ Size notion.
                -> b              -- ^ Shallowness.
                -> a              -- ^ Formal z parameter.
                -> MixedSystem a  -- ^ The computed Boltzmann system.

boltzmannSystem m h z = MixedSystem { shallowSystem = sys, plainSystem = sys' }
    where sys  = S.mixedBoltzmannSystem m h z
          sys' = P.boltzmannSystem m z

-- | Boltzmann sampler specification consisting of a Boltzmann system
--   with a corresponding size notion model.
data MixedSampler a b = MixedSampler { system :: MixedSystem a  -- ^ Boltzmann system.
                                     , model  :: Model b        -- ^ Size notion.
                                     }


-- | Computes the Boltzmann sampler specification
--   for closed lambda terms evaluated in the given parameter.
boltzmannSampler :: (Floating a, Integral b)
                 => Model b           -- ^ Size notion.
                 -> b                 -- ^ Shallowness.
                 -> a                 -- ^ Formal z parameter.
                 -> MixedSampler a b  -- ^ The computed Boltzmann sampler.

boltzmannSampler m h z = let sys = boltzmannSystem m h z in
                             MixedSampler { system = sys
                                          , model = m
                                          }

-- | Computes the rejection Boltzmann sampler for closed
--   lambda terms evaluated near the dominating singularity.
rejectionSampler :: (Floating a, Ord a, Integral b)
                 => Model b           -- ^ Size notion.
                 -> b                 -- ^ Shallowness.
                 -> a                 -- ^ Singularity approximation error.
                 -> MixedSampler a b  -- ^ The computed rejection Boltzmann sampler.

rejectionSampler m h eps = boltzmannSampler m h rho
    where rho = domSing m eps
