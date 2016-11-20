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
        ]
    ]
