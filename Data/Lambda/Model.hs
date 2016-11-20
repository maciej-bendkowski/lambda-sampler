{-|
 - Module      : Data.Lambda.Model 
 - Description : Basic lambda term size model notions.
 - Copyright   : (c) Maciej Bendkowski, 2016
 - 
 - License     : BSD3
 - Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 - Stability   : experimental
 -}
module Data.Lambda.Model
    ( -- * Size notions.
      Model(..)
    , natural, binary
    , valid

      -- * Helpers
    , weights
    ) where

-- | Lambda term size model.
data Model a = Model { zeroW :: a -- ^ Size of zero (a)
                     , succW :: a -- ^ Size of successor (b)
                     , absW  :: a -- ^ Size of the abstraction (c)
                     , appW  :: a -- ^ Size of the application (d)
                     }

-- | Checks whether the given size model is valid
--   in the framework of Gittenberger and Gołębiewski.
valid :: Integral a 
      => Model a    -- * Size notion
      -> Bool       -- * Whether the size model is valid.

valid m = a + d >= 1
       && b >= 1
       && c >= 1
       && gcd' [b, c, a+d] >= 1
    where (a,b,c,d) = weights m

gcd' [] = 1
gcd' [x] = x
gcd' (x:xs) = gcd x (gcd' xs)

-- | The `natural' size notion.
natural :: Integral a => Model a
natural = Model { zeroW = 1
                , succW = 1
                , absW  = 1
                , appW  = 1 
                }

-- | The `binary' size notion.
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
