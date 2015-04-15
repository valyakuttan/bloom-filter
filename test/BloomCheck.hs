
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where


import Data.Word (Word32)
import qualified Data.ByteString                      as Strict
import qualified Data.ByteString.Lazy                 as Lazy
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import qualified Data.BloomFilter.Easy                as B
import           Data.BloomFilter.Hash                (Hashable)
import           QCSupport

main :: IO ()
main = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions
  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts {
    topt_maximum_generated_tests = Just 1000
  }
  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }
  defaultMainWithOpts tests my_runner_opts

tests = [
          testGroup "Test Group 1"
              [
                testProperty "1" (prop_one_present (undefined :: Int))
              , testProperty "2" (prop_one_present (undefined :: Double))
              , testProperty "3" (prop_one_present (undefined :: Char))
              , testProperty "4" (prop_one_present (undefined :: String))
              , testProperty "5"
                (prop_one_present (undefined :: Lazy.ByteString))
              , testProperty "6"
                (prop_one_present (undefined :: Strict.ByteString))
              ]
        , testGroup "Test Group 2"
              [
                testProperty "1" (prop_all_present (undefined :: Int))
              , testProperty "2" (prop_all_present (undefined :: String))
              , testProperty "3"
                (prop_all_present (undefined :: Lazy.ByteString))
              , testProperty "4"
                (prop_all_present (undefined :: Strict.ByteString))
              ]
        , testProperty "sizing" prop_suggestions_sane
        ]

falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon)
    where epsilon = 1e-6

(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

prop_one_present :: (Hashable a, Eq a) => a -> P -> a -> Bool
prop_one_present _ (P p) elt =
    B.easyList p [elt] =~> \filt -> elt `B.elem` filt

prop_all_present :: Hashable a => a -> P -> [a] -> Bool
prop_all_present _ (P p) xs =
    B.easyList p xs =~> \filt -> all (`B.elem` filt) xs

prop_suggestions_sane :: P -> Property
prop_suggestions_sane (P p) =
    forAll (choose (1,fromIntegral maxWord32 `div` 8)) $ \cap ->
        let size = fst . minimum $ B.sizings cap p
        in size < fromIntegral maxWord32 ==>
           either (const False) sane $ B.suggestSizing cap p
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32 = maxBound :: Word32
