module Utils
    ( -- * Closed h-shallow lambda terms
      nat_prop_is_closed5
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

     -- * Closed lambda terms
    , nat_closed_prop_is_closed5
    , nat_closed_prop_is_closed15
    , nat_closed_prop_is_closed30

    , bin_closed_prop_is_closed5
    , bin_closed_prop_is_closed15
    , bin_closed_prop_is_closed30

      -- * Plain lambda terms
    , plain_nat_prop_sizeOK
    , plain_bin_prop_sizeOK

    , plain_nat_prop_filter_closed
    , plain_bin_prop_filter_closed
    ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Lambda
import Data.Lambda.Model

import Data.Lambda.Random
import qualified Data.Lambda.Random.System as S
import qualified Data.Lambda.Random.PlainSystem as P
import qualified Data.Lambda.Random.MixedSystem as M

natSampler :: Int -> S.Sampler Double Int
natSampler h = S.rejectionSampler natural h 1.0e-9

natClosedSampler :: Int -> M.MixedSampler Double Int
natClosedSampler h = M.rejectionSampler natural h 1.0e-9

binClosedSampler :: Int -> M.MixedSampler Double Int
binClosedSampler h = M.rejectionSampler binary h 1.0e-9

plainNatSampler :: P.PlainSampler Double Int
plainNatSampler = P.rejectionSampler natural 1.0e-9

binSampler :: Int -> S.Sampler Double Int
binSampler h = S.rejectionSampler binary h 1.0e-9

plainBinSampler :: P.PlainSampler Double Int
plainBinSampler = P.rejectionSampler binary 1.0e-9

natSys5 = natSampler 5
natSys15 = natSampler 15
natSys30 = natSampler 30

binSys5 = binSampler 5
binSys15 = binSampler 15
binSys30 = binSampler 30

sampler :: S.Sampler Double Int -> IO Lambda
sampler s = closedShallowLambdaIO s 500 50000

plainSampler :: P.PlainSampler Double Int -> IO Lambda
plainSampler s = plainLambdaIO s 500 50000

closedSampler :: M.MixedSampler Double Int -> IO Lambda
closedSampler s = closedLambdaIO s 500 50000

natLambdaSampler = plainSampler plainNatSampler
binLambdaSampler = plainSampler plainBinSampler

natSampler5 = sampler natSys5
natSampler15 = sampler natSys15
natSampler30 = sampler natSys30

binSampler5 = sampler binSys5
binSampler15 = sampler binSys15
binSampler30 = sampler binSys30

natClosedSys5 = natClosedSampler 5
natClosedSys15 = natClosedSampler 15
natClosedSys30 = natClosedSampler 30

binClosedSys5 = binClosedSampler 5
binClosedSys15 = binClosedSampler 15
binClosedSys30 = binClosedSampler 30

natClosedSampler5 = closedSampler natClosedSys5
natClosedSampler15 = closedSampler natClosedSys15
natClosedSampler30 = closedSampler natClosedSys30

binClosedSampler5 = closedSampler binClosedSys5
binClosedSampler15 = closedSampler binClosedSys15
binClosedSampler30 = closedSampler binClosedSys30

prop sampler f = monadicIO $ do
    t <- run sampler
    assert $ f t

-- Closed h-shallow lambda terms
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

-- Closed lambda terms
nat_closed_prop_is_closed5 = prop natClosedSampler5 isClosed
nat_closed_prop_is_closed15 = prop natClosedSampler15 isClosed
nat_closed_prop_is_closed30 = prop natClosedSampler30 isClosed

bin_closed_prop_is_closed5 = prop binClosedSampler5 isClosed
bin_closed_prop_is_closed15 = prop binClosedSampler15 isClosed
bin_closed_prop_is_closed30 = prop binClosedSampler30 isClosed

-- Plain lambda terms
plain_nat_prop_sizeOK = prop natLambdaSampler (sizeOK natural)
plain_bin_prop_sizeOK = prop binLambdaSampler (sizeOK binary)

filterSampler f s = filterPlainIO f s 100 10000

plainNatIsClosedFilter = filterSampler isClosed plainNatSampler
plainBinIsClosedFilter = filterSampler isClosed plainBinSampler

plain_nat_prop_filter_closed = prop plainNatIsClosedFilter isClosed
plain_bin_prop_filter_closed = prop plainBinIsClosedFilter isClosed
