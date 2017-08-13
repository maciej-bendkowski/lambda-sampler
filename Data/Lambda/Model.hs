{-|
 Module      : Data.Lambda.Model
 Description : Basic lambda term size model notions.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Size notion framework of Gittenberger and Gołębiewski for lambda terms
 in the de Bruijn notation. A size definition constitutes the weights
 for lambda abstraction, term application, successor and zero.
 -}
module Data.Lambda.Model
    ( -- * Size notions
      Model(..)
    , natural, binary
    , valid

      -- * Helpers
    , weights
    ) where

-- | Lambda term size model.
data Model a = Model { zeroW :: a -- ^ Size of zero
                     , succW :: a -- ^ Size of successor
                     , absW  :: a -- ^ Size of the abstraction
                     , appW  :: a -- ^ Size of the application
                     }

-- | Checks whether the given size model is valid
--   in the framework of Gittenberger and Gołębiewski.
valid :: Integral a
      => Model a    -- ^ Size notion
      -> Bool       -- ^ Whether the size model is valid.

valid m = a + d >= 1
       && b >= 1
       && c >= 1
       && gcd' [b, c, a+d] >= 1
    where (a,b,c,d) = weights m

gcd' :: Integral a => [a] -> a

gcd' [] = 1
gcd' [x] = x
gcd' (x:xs) = gcd x (gcd' xs)

-- | The natural size notion.
natural :: Integral a => Model a
natural = Model { zeroW = 1
                , succW = 1
                , absW  = 1
                , appW  = 1
                }

-- | The binary size notion.
binary :: Integral a => Model a
binary = Model { zeroW = 2
               , succW = 1
               , absW  = 2
               , appW  = 2
               }

-- | Given a size notion, returns a tuple (a,b,c,d) where a denotes the size
--   of the de Bruijn zero, b denotes the size of the de Bruijn successor,
--   c denotes the size of the lambda abstraction and finally d denotes the
--   size of the application.
weights :: Model a
        -> (a,a,a,a)

weights m = (zeroW m,
             succW m,
             absW m,
             appW m)
