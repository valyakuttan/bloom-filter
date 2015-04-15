module Data.BloomFilter.Mutable
    (
      MBloom
    , elem
    , notElem
    , insert
    , length
    , new
    ) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)
import qualified Data.Vector.Unboxed.Mutable as V

import Data.BloomFilter.Mutable.Internal (Hash, MBloom(..))


elem :: a -> MBloom s a -> ST s Bool
elem elt filt = allM readV $ map ix $ hashes filt elt
  where
      readV = V.read (bitArray filt) 
      ix = fromIntegral . (`mod` length filt)

notElem :: a -> MBloom s a -> ST s Bool
notElem elt filt = not `liftM` elem elt filt

insert :: MBloom s a -> a -> ST s ()
insert filt elt = mapM_ (\i -> writeV i True) $ hashes filt elt
  where
      writeV i = V.write (bitArray filt) $ ix i
      ix = fromIntegral . (`mod` length filt)

length :: MBloom s a -> Word32
length = fromIntegral . V.length . bitArray

new :: (a -> [Hash]) -> Word32 -> ST s (MBloom s a)
new hash numBits = MB hash `liftM` vec
  where vec = V.replicate (fromIntegral numBits) False

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x:xs) = do
  ok <- p x
  if ok
    then allM p xs
    else return False
allM _ [] = return True
