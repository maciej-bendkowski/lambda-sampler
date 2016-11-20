module Utils
    ( nat_prop_is_closed5
    , nat_prop_is_closed15 
    , nat_prop_is_closed30
    
    , bin_prop_is_closed5
    , bin_prop_is_closed15 
    , bin_prop_is_closed30
    
    , nat_prop_size5
    , nat_prop_size15
    , nat_prop_size30
    
    , bin_prop_size5
    , bin_prop_size15
    , bin_prop_size30
    
    , nat_prop_idx5
    , nat_prop_idx15
    , nat_prop_idx30
    
    , bin_prop_idx5
    , bin_prop_idx15
    , bin_prop_idx30
    ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Lambda
import Data.Lambda.Model

import Data.Lambda.Random
import Data.Lambda.Random.System

natSampler :: Int -> Sampler Double Int
natSampler h = rejectionSampler natural h 1.0e-9

binSampler :: Int -> Sampler Double Int
binSampler h = rejectionSampler binary h 1.0e-9

natSys5 = natSampler 5
natSys15 = natSampler 15
natSys30 = natSampler 30

binSys5 = binSampler 5
binSys15 = binSampler 15
binSys30 = binSampler 30

sampler :: Sampler Double Int -> IO Lambda
sampler s = closedLambdaIO s 500 50000 

natSampler5 = sampler natSys5
natSampler15 = sampler natSys15
natSampler30 = sampler natSys30

binSampler5 = sampler binSys5
binSampler15 = sampler binSys15
binSampler30 = sampler binSys30

prop sampler f = monadicIO $ do
    t <- run sampler
    assert $ f t

nat_prop_is_closed5 = prop natSampler5 isClosed
nat_prop_is_closed15 = prop natSampler15 isClosed
nat_prop_is_closed30 = prop natSampler30 isClosed

bin_prop_is_closed5 = prop binSampler5 isClosed
bin_prop_is_closed15 = prop binSampler15 isClosed
bin_prop_is_closed30 = prop binSampler30 isClosed

sizeOK model x = 500 <= s && s <= 50000
    where s = size model x

nat_prop_size5 = prop natSampler5 (sizeOK natural)
nat_prop_size15 = prop natSampler15 (sizeOK natural)
nat_prop_size30 = prop natSampler30 (sizeOK natural)

bin_prop_size5 = prop binSampler5 (sizeOK binary)
bin_prop_size15 = prop binSampler15 (sizeOK binary)
bin_prop_size30 = prop binSampler30 (sizeOK binary)

indexOK ub x = 0 <= idx && idx < ub
    where idx = maxIndex x

nat_prop_idx5 = prop natSampler5 (indexOK 5)
nat_prop_idx15 = prop natSampler15 (indexOK 15)
nat_prop_idx30 = prop natSampler30 (indexOK 30)

bin_prop_idx5 = prop binSampler5 (indexOK 5)
bin_prop_idx15 = prop binSampler15 (indexOK 15)
bin_prop_idx30 = prop binSampler30 (indexOK 30)
