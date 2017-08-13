{-|
 Module      : Data.Lambda.Random.System
 Description : Basic notions regarding combinatorial systems
               defining closed h-shallow lambda terms.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Combinatorial system defining closed h-shallow lambda terms in the de Bruijn
notation.
-}
module Data.Lambda.Random.System
    ( -- * System
      Expr(..)
    , System
    , boltzmannSystem
    , mixedBoltzmannSystem

     -- * Boltzmann samplers
    , Sampler(..)
    , boltzmannSampler
    , rejectionSampler
    ) where

import Prelude hiding (abs)

import Data.Lambda ()
import Data.Lambda.Model
import Data.Lambda.Random.Oracle

import qualified Data.Lambda.Random.PlainSystem as P

-- | An expression defining the branching probabilities
--   in the Boltzmann model for closed h-shallow lambda terms.
data Expr a = Expr { abs :: a   -- ^ Abstraction probability.
                   , app :: a   -- ^ Application probability.
                   , idx :: [a] -- ^ Probabilities for de Bruijn indices
                   } deriving (Show)

-- | Combinatorial system specification
--   for closed h-shallow lambda terms.
type System a = [Expr a]

evalH :: (Floating a, Integral b)
      => Model b -> b -> a -> a

evalH m h z = (1-z^^c-sqrt ((z^^c-1)^^2
    -(4*z^^(a+d)*(z^^(b*h)-1))/(z^^b-1)))/(2*z^^d)
    where (a,b,c,d) = weights m

evalI :: (Floating a, Integral b)
      => Model b -> b -> a -> a -> a

evalI m i p z = (1-sqrt (1-4*z^^d*(p*z^^c
    +(z^^a*(z^^(b*i)-1))/(z^^b-1))))/(2*z^^d)
    where (a,b,c,d) = weights m

evalS :: (Floating a, Integral b)
      => Model b -> a -> a -> a

evalS m p z = (1-sqrt (1-4*p*z^^(c+d)))/(2*z^^d)
    where (_,_,c,d) = weights m

take' :: (Integral a)
      => a -> [b] -> [b]

take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs
take' _ [] = error "Empty list!"

computeIdx :: (Floating a, Integral b)
           => Model b -> b -> a -> a -> [a]

computeIdx _ 0 _ _ = []
computeIdx m h z w = take' h $ map (/ w) idxSeq
    where idxSeq = z^^a : next idxSeq

          next (x:xs) = x * z^^b : next xs
          next _ = error "Finite list!"

          (a,b,_,_) = weights m

-- | Computes the Boltzmann model for closed h-shallow
--   lambda terms evaluated in the given parameter.
boltzmannSystem :: (Floating a, Integral b)
                => Model b      -- ^ Size notion.
                -> b            -- ^ Shallowness.
                -> a            -- ^ Formal z parameter.
                -> System a     -- ^ The computed Boltzmann system.

boltzmannSystem m h z = map toProb sys
    where sys = snd $ computeSys' (evalH m h) m h 0 z

-- | Computes the mixed Boltzmann model for closed
--   lambda terms evaluated in the given parameter.
mixedBoltzmannSystem :: (Floating a, Integral b)
                => Model b      -- ^ Size notion.
                -> b            -- ^ Shallowness.
                -> a            -- ^ Formal z parameter.
                -> System a     -- ^ The computed Boltzmann system.

mixedBoltzmannSystem m h z = map toProb sys
    where sys = snd $ computeSys' (P.eval m) m h 0 z

computeSys' :: (Floating a, Integral b)
            => (a -> a) -> Model b -> b -> b -> a
            -> (a, System a)

computeSys' eval m h i z
  | i == 0 = let
    w = evalS m w' z
    (_,_,c,d) = weights m
    (w',c1) = computeSys' eval m h 1 z
    in (w, Expr { abs = (w' * z^^c) / w
                , app = w * z^^d
                , idx = computeIdx m 0 z w
                } : c1)

  | i == h = let
    w = eval z
    (_,_,c,d) = weights m
    in (w, [Expr { abs = z^^c
                 , app = w * z^^d
                 , idx = computeIdx m h z w
                 }])

  | otherwise = let
    w = evalI m i w' z
    (_,_,c,d) = weights m
    (w',cp) = computeSys' eval m h (i+1) z
     in (w, Expr { abs = (w' * z^^c) / w
                 , app = w * z^^d
                 , idx = computeIdx m i z w
                 } : cp)

toProbIdx :: (Num a)
          => a -> [a] -> [a]

toProbIdx _ [] = []
toProbIdx w (x:xs) = w' : toProbIdx w' xs
    where w' = w + x

toProb :: (Num a)
       => Expr a -> Expr a

toProb expr = expr { app = x
                   , idx = idxs'
                   }
    where app' = app expr
          idxs' = toProbIdx x (idx expr)
          x = abs expr + app'

-- | Boltzmann sampler specification consisting of a Boltzmann system
--   with a corresponding size notion model.
data Sampler a b = Sampler { system :: System a     -- ^ Boltzmann system.
                           , model :: Model b       -- ^ Size notion.
                           }

-- | Computes the Boltzmann sampler specification
--   for closed h-shallow lambda terms evaluated in the given parameter.
boltzmannSampler :: (Floating a, Integral b)
                 => Model b         -- ^ Size notion.
                 -> b               -- ^ Shallowness.
                 -> a               -- ^ Formal z parameter.
                 -> Sampler a b     -- ^ The computed Boltzmann sampler.

boltzmannSampler m h z = let sys = boltzmannSystem m h z in
                             Sampler { system = sys
                                     , model = m
                                     }

-- | Computes the rejection Boltzmann sampler for closed h-shallow
--   lambda terms evaluated near the dominating singularity.
rejectionSampler :: (Floating a, Ord a, Integral b)
                 => Model b         -- ^ Size notion.
                 -> b               -- ^ Shallowness.
                 -> a               -- ^ Singularity approximation error.
                 -> Sampler a b     -- ^ The computed rejection Boltzmann sampler.

rejectionSampler m h eps = boltzmannSampler m h rho
    where rho = domSingH m h eps
