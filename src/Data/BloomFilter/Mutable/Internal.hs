-- |
-- Bloom filter internal

module Data.BloomFilter.Mutable.Internal
    (
    -- * Types
      Hash
    , MBloom(..)
    ) where


import Data.Word (Word32)
import qualified Data.Vector.Unboxed.Mutable as V


type Hash = Word32

-- | A mutable Bloom filter, for use within the 'ST' monad.
data MBloom s a = MB
    { hashes :: !(a -> [Hash])
    , bitArray :: {-# UNPACK #-} !(V.MVector s Bool)
    }
