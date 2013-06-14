{-# LANGUAGE CPP #-}

module Data.HashTable.Internal.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray

  , InterleavedArray
  , newInterleaved
  , readKey
  , writeKey
  , readValue
  , writeValue
  ) where

------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Control.Monad.ST
#ifdef BOUNDS_CHECKING
import           Data.Vector.Mutable           (MVector)
import qualified Data.Vector.Mutable           as M
#else
import           Data.Primitive.Array          (MutableArray)
import qualified Data.Primitive.Array          as M
#endif
import           GHC.Exts
import           Unsafe.Coerce
------------------------------------------------------------------------------
import           Data.HashTable.Internal.Utils


------------------------------------------------------------------------------
#ifdef BOUNDS_CHECKING
type MutableArray s a = MVector s a

newArray :: Int -> a -> ST s (MutableArray s a)
newArray = M.replicate

readArray :: MutableArray s a -> Int -> ST s a
readArray = M.read

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = M.write


------------------------------------------------------------------------------
#else
newArray :: Int -> a -> ST s (MutableArray s a)
newArray = M.newArray

readArray :: MutableArray s a -> Int -> ST s a
readArray = M.readArray

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = M.writeArray

#endif


------------------------------------------------------------------------------
type InterleavedArray s k v = MutableArray s Any


------------------------------------------------------------------------------
-- We kind of cheat here: we know we don't care about the values if the keys
-- are not set, so we just set the key everywhere
newInterleaved :: Int -> k -> ST s (InterleavedArray s k v)
newInterleaved n k = newArray (n `iShiftL` 1) (unsafeCoerce k)


------------------------------------------------------------------------------
readKey :: InterleavedArray s k v -> Int -> ST s k
readKey arr i = unsafeCoerce <$> (M.readArray arr $! i `iShiftL` 1)

------------------------------------------------------------------------------
readValue :: InterleavedArray s k v -> Int -> ST s v
readValue arr i = unsafeCoerce <$> (M.readArray arr $! i `iShiftL` 1 + 1)

------------------------------------------------------------------------------
writeKey :: InterleavedArray s k v -> Int -> k -> ST s ()
writeKey arr i k = M.writeArray arr (i `iShiftL` 1) (unsafeCoerce k)

------------------------------------------------------------------------------
writeValue :: InterleavedArray s k v -> Int -> v -> ST s ()
writeValue arr i v = M.writeArray arr (i `iShiftL` 1 + 1) (unsafeCoerce v)
