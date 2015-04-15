module Data.BloomFilter.Easy
    (
      suggestSizing
    , sizings
    , easyList
    -- re-export useful names from BloomFilter
    , B.Bloom
    , B.length
    , B.elem
    , B.notElem
    ) where

import           Data.BloomFilter.Hash (Hashable, cheapHashes)
import           Data.Maybe            (catMaybes)
import           Data.Word             (Word32)

import qualified Data.BloomFilter      as B

easyList :: (Hashable a)
         => Double             -- false positive rate (between 0 and 1)
         -> [a]                -- values to populate the filter with
         -> Either String (B.Bloom a)
easyList errRate values =
    case suggestSizing (length values) errRate of
      Left err -> Left err
      Right (bits,hashes) -> Right filt
        where filt = B.fromList (cheapHashes hashes) bits values

suggestSizing :: Int   -- expected maximum capacity
              -> Double    -- desired false positive rate
              -> Either String (Word32,Int) -- (filter size,
                                            --  number of hashes)
suggestSizing capacity errRate
    | capacity <= 0 = Left "capacity too small"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | null saneSizes = Left "capacity too large"
    | otherwise = Right (minimum saneSizes)
  where saneSizes = catMaybes . map sanitize $ sizings capacity errRate
        sanitize (bits,hashes)
            | bits > maxWord32 - 1 = Nothing
            | otherwise = Just (ceiling bits, truncate hashes)
            where maxWord32 = fromIntegral (maxBound :: Word32)

sizings :: Int -> Double -> [(Double, Double)]
sizings capacity errRate =
    [(((-k) * cap / log (1 - (errRate ** (1 / k)))), k) | k <- [1..50]]
  where cap = fromIntegral capacity
