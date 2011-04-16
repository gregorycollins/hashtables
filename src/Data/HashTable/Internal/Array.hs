{-# LANGUAGE CPP #-}

module Data.HashTable.Internal.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray
  ) where


import           Control.Monad.ST
#ifdef BOUNDS_CHECKING
import qualified Data.Vector.Mutable as M
import           Data.Vector.Mutable (MVector)
#else
import qualified Data.Primitive.Array as M
import           Data.Primitive.Array (MutableArray)
#endif


#ifdef BOUNDS_CHECKING

type MutableArray s a = MVector s a

newArray :: Int -> a -> ST s (MutableArray s a)
newArray = M.replicate

readArray :: MutableArray s a -> Int -> ST s a
readArray = M.read

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = M.write

#else

newArray :: Int -> a -> ST s (MutableArray s a)
newArray = M.newArray

readArray :: MutableArray s a -> Int -> ST s a
readArray = M.readArray

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = M.writeArray

#endif
