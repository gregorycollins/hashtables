{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Data.HashTable.Internal.IntArray
  ( IntArray
  , newArray
  , readArray
  , writeArray
  , length
  , toPtr
  ) where

import           Control.Monad.ST
import           Data.Bits
import qualified Data.Primitive.ByteArray as A
import           Data.Primitive.Types (Addr(..))
import           GHC.Exts
import           Prelude hiding (length)

#ifdef BOUNDS_CHECKING
#define BOUNDS_MSG(sz,i) concat [ "[", __FILE__, ":", \
                                  show (__LINE__ :: Int), \
                                  "] bounds check exceeded: ",\
                                  "size was ", show (sz), " i was ", show (i) ]
#define BOUNDS_CHECK(arr,i) let sz = (A.sizeofMutableByteArray (arr) \
                                      `div` wordSizeInBytes) in \
                            if (i) < 0 || (i) >= sz \
                              then error (BOUNDS_MSG(sz,(i))) \
                              else return ()
#else
#define BOUNDS_CHECK(arr,i)
#endif

newtype IntArray s = IA (A.MutableByteArray s)


wordSizeInBytes :: Int
wordSizeInBytes = bitSize (0::Int) `div` 8


-- | Cache line size, in bytes
cacheLineSize :: Int
cacheLineSize = 64


newArray :: Int -> ST s (IntArray s)
newArray n = do
    let !sz = n * wordSizeInBytes
    !arr <- A.newAlignedPinnedByteArray sz  cacheLineSize
    A.fillByteArray arr 0 sz 0
    return $! IA arr


readArray :: IntArray s -> Int -> ST s Int
readArray (IA a) idx = do
    BOUNDS_CHECK(a,idx)
    A.readByteArray a idx


writeArray :: IntArray s -> Int -> Int -> ST s ()
writeArray (IA a) idx val = do
    BOUNDS_CHECK(a,idx)
    A.writeByteArray a idx val


length :: IntArray s -> Int
length (IA a) = A.sizeofMutableByteArray a `div` wordSizeInBytes


toPtr :: IntArray s -> Ptr a
toPtr (IA a) = Ptr a#
  where
    !(Addr !a#) = A.mutableByteArrayContents a
