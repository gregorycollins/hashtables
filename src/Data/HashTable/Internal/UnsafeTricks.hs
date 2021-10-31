{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
#ifdef UNSAFETRICKS
{-# LANGUAGE MagicHash    #-}
#endif

module Data.HashTable.Internal.UnsafeTricks
  ( Key
  , toKey
  , fromKey
  , emptyRecord
  , deletedRecord
  , keyIsEmpty
  , keyIsDeleted
  , writeDeletedElement
  , makeEmptyVector
  ) where

import           Control.Monad.Primitive
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
#ifdef UNSAFETRICKS
import           GHC.Exts
import           Unsafe.Coerce
#endif


------------------------------------------------------------------------------
#ifdef UNSAFETRICKS
type Key a = Any

#else
data Key a = Key !a
           | EmptyElement
           | DeletedElement
  deriving (Show)
#endif


------------------------------------------------------------------------------
-- Type signatures
emptyRecord :: Key a
deletedRecord :: Key a
keyIsEmpty :: Key a -> Bool
keyIsDeleted :: Key a -> Bool
makeEmptyVector :: PrimMonad m => Int -> m (MVector (PrimState m) (Key a))
writeDeletedElement :: PrimMonad m =>
                       MVector (PrimState m) (Key a) -> Int -> m ()
toKey :: a -> Key a
fromKey :: Key a -> a


#ifdef UNSAFETRICKS
data TombStone = EmptyElement
               | DeletedElement

{-# NOINLINE emptyRecord #-}
emptyRecord = unsafeCoerce EmptyElement

{-# NOINLINE deletedRecord #-}
deletedRecord = unsafeCoerce DeletedElement

{-# INLINE keyIsEmpty #-}
keyIsEmpty a = isTrue# (x# ==# 1#)
  where
    !x# = reallyUnsafePtrEquality# a emptyRecord

{-# INLINE keyIsDeleted #-}
keyIsDeleted a = isTrue# (x# ==# 1#)
  where
    !x# = reallyUnsafePtrEquality# a deletedRecord

{-# INLINE toKey #-}
toKey = unsafeCoerce

{-# INLINE fromKey #-}
fromKey = unsafeCoerce

#else

emptyRecord = EmptyElement

deletedRecord = DeletedElement

keyIsEmpty EmptyElement = True
keyIsEmpty _            = False

keyIsDeleted DeletedElement = True
keyIsDeleted _              = False

toKey = Key

fromKey (Key x) = x
fromKey _ = error "impossible"

#endif


------------------------------------------------------------------------------
{-# INLINE makeEmptyVector #-}
makeEmptyVector m = M.replicate m emptyRecord

------------------------------------------------------------------------------
{-# INLINE writeDeletedElement #-}
writeDeletedElement v i = M.unsafeWrite v i deletedRecord
