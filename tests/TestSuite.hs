module Main
    ( main
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Utils

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
        testGroup "Natural size notion"
        [
           testProperty "5-shallow terms are closed" nat_prop_is_closed5
         , testProperty "15-shallow terms are closed" nat_prop_is_closed15
         , testProperty "30-shallow terms are closed" nat_prop_is_closed30

         , testProperty "5-shallow terms are of correct size" nat_prop_size5
         , testProperty "15-shallow terms are of correct size" nat_prop_size15
         , testProperty "30-shallow terms are of correct size" nat_prop_size30

         , testProperty "5-shallow terms have indices at most 5" nat_prop_idx5
         , testProperty "15-shallow terms have indices at most 15" nat_prop_idx15
         , testProperty "30-shallow terms have indices at most 30" nat_prop_idx30

         , testProperty "Closed terms are closed (5-approx)" nat_closed_prop_is_closed5
         , testProperty "Closed terms are closed (15-approx)" nat_closed_prop_is_closed15
         , testProperty "Closed terms are closed (30-approx)" nat_closed_prop_is_closed30

         , testProperty "Plain terms are of correct size" plain_nat_prop_sizeOK
         , testProperty "isClosed filter returns closed lambda terms" plain_nat_prop_filter_closed
        ],

        testGroup "Binary size notion"
        [
           testProperty "5-shallow terms are closed" bin_prop_is_closed5
         , testProperty "15-shallow terms are closed" bin_prop_is_closed15
         , testProperty "30-shallow terms are closed" bin_prop_is_closed30

         , testProperty "5-shallow terms are of correct size" bin_prop_size5
         , testProperty "15-shallow terms are of correct size" bin_prop_size15
         , testProperty "30-shallow terms are of correct size" bin_prop_size30

         , testProperty "5-shallow terms have indices at most 5" bin_prop_idx5
         , testProperty "15-shallow terms have indices at most 15" bin_prop_idx15
         , testProperty "30-shallow terms have indices at most 30" bin_prop_idx30

         , testProperty "Closed terms are closed (5-approx)" bin_closed_prop_is_closed5
         , testProperty "Closed terms are closed (15-approx)" bin_closed_prop_is_closed15
         , testProperty "Closed terms are closed (30-approx)" bin_closed_prop_is_closed30

         , testProperty "Plain terms are of correct size" plain_bin_prop_sizeOK
         , testProperty "isClosed filter returns closed lambda terms" plain_bin_prop_filter_closed
        ]
    ]
