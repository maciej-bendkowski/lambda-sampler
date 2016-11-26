{-|
 Module      : Data.Lambda 
 Description : Lambda terms in the de Bruijn notation.
 Copyright   : (c) Maciej Bendkowski, 2016
 
 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Lambda terms in the de Bruijn notation where instead of variables, we use
 natural indices encoded as a sequence of successors of zero. 
     
 Each index captures the distance to its binding lambda - Z corresponds to a variable
 bound by the first lambda abstraction on its way to the term root. The successor S
 increases the distance of its argument by one. If an index points to a lambda
 abstraction outside of the term, then that index represents a free variable 
 in the term. Such a representation for lambda terms avoids using explicit
 variable names and hence eliminates the use of alpha-conversion.
 -}
module Data.Lambda
    ( -- * Lambda terms
      Lambda(..)
    , isClosed, size, sizeVar, maxIndex

      -- * de Bruijn indices
    , Index(..)
    , toInt, toIndex
    ) where

import Data.Lambda.Model

-- | de Bruijn indices in unary notation.
data Index = S Index
           | Z

-- | Translates the given index to a corresponding positive integer.
toInt :: (Num a)
      => Index
      -> a

toInt Z = 0
toInt (S n) = 1 + toInt n

-- | Translates the given positive integer to a corresponding index.
toIndex :: (Num a, Eq a)
        => a
        -> Index

toIndex 0 = Z
toIndex n = S $ toIndex (n-1)

instance Show Index where
    showsPrec _ = shows . toInt

-- | Lambda terms with de Bruijn indices.
data Lambda = Var Index         -- ^ de Bruijn indices.
            | Abs Lambda        -- ^ Lambda abstraction.
            | App Lambda Lambda -- ^ Term application.

instance Show Lambda where
    showsPrec _ (Var n) = shows n
    showsPrec _ (Abs lt) = (:) 'λ' . shows lt
    showsPrec _ (App lt rt) = (:) '(' . shows lt . (:) ')' .  (:) '(' . shows rt . (:) ')'

-- | Predicate defining closed lambda terms.
isClosed :: Lambda 
         -> Bool

isClosed = isClosed' 0 
    where
        isClosed' h (Abs t) = isClosed' (h+1) t
        isClosed' h (App t t') = isClosed' h t && isClosed' h t'
        isClosed' h (Var n) = toInt n < h

-- | Finds the maximal index in the given lambda term.
maxIndex :: (Num a, Ord a) 
         => Lambda
         -> a

maxIndex (Abs t) = maxIndex t
maxIndex (App t t') = max (maxIndex t) (maxIndex t')
maxIndex (Var n) = toInt n

-- | Computes the size of the given lambda term.
size :: Num a 
     => Model a -- ^ Size notion used in the computations.
     -> Lambda  -- ^ The considered lambda term.
     -> a       -- ^ The integer size of the lambda term.

size m (Abs t) = absW m + size m t
size m (App t t') = appW m + size m t + size m t'
size m (Var n) = sizeVar m n

-- | Computes the size of the given de Bruijn index.
sizeVar :: Num a 
        => Model a  -- ^ Size notion used in the computations.
        -> Index    -- ^ The considered de Bruijn index.
        -> a        -- ^ The integer size of the lambda term.

sizeVar m (S n) = succW m + sizeVar m n
sizeVar m Z = zeroW m
