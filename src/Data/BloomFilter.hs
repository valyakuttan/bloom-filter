
module Data.BloomFilter
    (
      Bloom
    , length
    , elem
    , notElem
    , fromList
    ) where

import Prelude hiding (elem, length, notElem)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as U

import Data.BloomFilter.Mutable (insert, new)
import Data.BloomFilter.Mutable.Internal (Hash, MBloom(..))


data Bloom a = B
    { blmHash  :: !(a -> [Hash])
    , blmArray :: U.Vector Bool
    }

length :: Bloom a -> Hash
length = fromIntegral . U.length . blmArray

elem :: a -> Bloom a -> Bool
elt `elem` filt   = all test $ blmHash filt elt
  where
      test hash = blmArray filt ! fromIntegral (hash `mod` length filt)

notElem :: a -> Bloom a -> Bool
elt `notElem` filt = not (elt `elem` filt)

fromList :: (a -> [Hash])    -- family of hash functions to use
         -> Hash             -- number of bits in filter
         -> [a]                -- values to populate with
         -> Bloom a
fromList hash numBits values = B hash $ U.create $ do
    v <- new hash numBits
    mapM_ (insert v) values
    return $ bitArray v
