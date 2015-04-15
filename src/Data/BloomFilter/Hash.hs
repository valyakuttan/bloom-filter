{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.BloomFilter.Hash
    (
      Hashable(..)
    , hash
    , doubleHash
    , cheapHashes
    ) where


import           Control.Monad         (foldM)
import           Data.Bits             (shiftR, (.&.))
import qualified Data.ByteString       as SB
import qualified Data.ByteString.Lazy  as LB
import           Data.Word             (Word32, Word64)
import           Foreign.C.Types
import           Foreign.Marshal.Array (withArrayLen)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr           (Ptr, castPtr, plusPtr)
import           Foreign.Storable      (Storable, peek, sizeOf)
import           System.IO.Unsafe      (unsafePerformIO)


foreign import ccall unsafe "lookup3.h hashword2" hashWord2
    :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
    :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

class Hashable a where
    hashSalt :: Word64       -- ^ salt
             -> a            -- ^ value to hash
             -> Word64

hash :: Hashable a => a -> Word64
hash = hashSalt 0x16fc397cf62f64d3

-- | Compute a list of 32-bit hashes relatively cheaply.  The value to
-- hash is inspected at most twice, regardless of the number of hashes
-- requested.
--
-- We use a variant of Kirsch and Mitzenmacher's technique from \"Less
-- Hashing, Same Performance: Building a Better Bloom Filter\",
-- <http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/esa06.pdf>.
--
-- Where Kirsch and Mitzenmacher multiply the second hash by a
-- coefficient, we shift right by the coefficient.  This offers better
-- performance (as a shift is much cheaper than a multiply), and the
-- low order bits of the final hash stay well mixed.
cheapHashes :: Hashable a => Int -- ^ number of hashes to compute
            -> a                 -- ^ value to hash
            -> [Word32]
cheapHashes k v = go 0
    where go i | i == j = []
               | otherwise = hsh : go (i + 1)
               where !hsh = h1 + (h2 `shiftR` i)
          h1 = fromIntegral (h `shiftR` 32)
          h2 = fromIntegral h
          h = hashSalt 0x9150a946c4a8966e v
          j = fromIntegral k

doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = go 0
    where go n | n == num  = []
               | otherwise = h1 + h2 * n : go (n + 1)

          !h1 = fromIntegral (h `shiftR` 32) .&. maxBound
          !h2 = fromIntegral h

          h   = hashSalt 0x9150a946c4a8966e value
          num = fromIntegral numHashes

hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k = unsafePerformIO . with k $ \ptr ->
                      hashIO ptr (fromIntegral (sizeOf k)) salt

instance Hashable Char where hashSalt = hashStorable
instance Hashable Int  where hashSalt = hashStorable
instance Hashable Double where hashSalt = hashStorable

hashList :: (Storable a) => Word64 -> [a] -> IO Word64
hashList salt xs =
    withArrayLen xs $ \len ptr ->
      hashIO ptr (fromIntegral (len * sizeOf x)) salt
  where x = head xs

instance (Storable a) => Hashable [a] where
    hashSalt salt xs = unsafePerformIO $ hashList salt xs

hash2 :: (Hashable a) => a -> Word64 -> Word64
hash2 = flip hashSalt

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hashSalt salt (a,b) = hash2 b . hash2 a $ salt

instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hashSalt salt (a,b,c) = hash2 c . hash2 b . hash2 a $ salt

hashByteString :: Word64 -> SB.ByteString -> IO Word64
hashByteString salt bs = SB.useAsCStringLen bs $ \(ptr, len) ->
    hashIO ptr (fromIntegral len) salt

instance Hashable SB.ByteString where
    hashSalt salt bs = unsafePerformIO $ hashByteString salt bs

rechunk :: LB.ByteString -> [SB.ByteString]
rechunk s
    | LB.null s = []
    | otherwise   = let (pre,suf) = LB.splitAt chunkSize s
                    in repack pre : rechunk suf

    where repack = SB.concat . LB.toChunks
          chunkSize = 64 * 1024

instance Hashable LB.ByteString where
    hashSalt salt bs = unsafePerformIO $
        foldM hashByteString salt (rechunk bs)

hashIO :: Ptr a -- value to hash
       -> CSize -- number of bytes
       -> Word64 -- salt
       -> IO Word64
hashIO ptr bytes salt =
    with (fromIntegral salt) $ \sp -> do
      let p1 = castPtr sp
          p2 = castPtr sp `plusPtr` 4
      go p1 p2
      peek sp
  where go p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) wds p1 p2
          | otherwise = hashLittle2 ptr bytes p1 p2
        wds = bytes `div` 4
