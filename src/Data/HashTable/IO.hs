{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}

-- | This module provides wrappers in 'IO' around the functions from
-- "Data.HashTable.Class".
--
-- This module exports three concrete hash table types, one for each hash table
-- implementation in this package:
--
-- > type BasicHashTable  k v = IOHashTable (B.HashTable)  k v
-- > type CuckooHashTable k v = IOHashTable (Cu.HashTable) k v
-- > type LinearHashTable k v = IOHashTable (L.HashTable)  k v
--
-- The 'IOHashTable' type can be thought of as a wrapper around a concrete
-- hashtable type, which sets the 'ST' monad state type to 'PrimState' 'IO',
-- a.k.a. 'RealWorld':
--
-- > type IOHashTable tabletype k v = tabletype (PrimState IO) k v
--
-- This module provides 'stToIO' wrappers around the hashtable functions (which
-- are in 'ST') to make it convenient to use them in 'IO'. It is intended to be
-- imported qualified and used with a user-defined type alias, i.e.:
--
-- > import qualified Data.HashTable.IO as H
-- >
-- > type HashTable k v = H.CuckooHashTable k v
-- >
-- > foo :: IO (HashTable Int Int)
-- > foo = do
-- >     ht <- H.new
-- >     H.insert ht 1 1
-- >     return ht
--
-- Essentially, anywhere you see @'IOHashTable' h k v@ in the type signatures
-- below, you can plug in any of @'BasicHashTable' k v@, @'CuckooHashTable' k
-- v@, or @'LinearHashTable' k v@.
--
module Data.HashTable.IO
  ( BasicHashTable
  , CuckooHashTable
  , LinearHashTable
  , IOHashTable
  , new
  , newSized
  , insert
  , delete
  , lookup
  , mutate
  , mutateIO
  , fromList
  , fromListWithSizeHint
  , toList
  , mapM_
  , foldM
  , computeOverhead
  , lookupIndex
  , nextByIndex
  ) where


------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Data.Word
#endif
import           Control.Monad.Primitive       (PrimState)
import           Control.Monad.ST              (stToIO)
import           Data.Hashable                 (Hashable)
import qualified Data.HashTable.Class          as C
import           GHC.IO                        (ioToST)
import           Prelude                       hiding (lookup, mapM_)

------------------------------------------------------------------------------
import           Data.HashTable.Internal.Utils (unsafeIOToST)
import qualified Data.HashTable.ST.Basic       as B
import qualified Data.HashTable.ST.Cuckoo      as Cu
import qualified Data.HashTable.ST.Linear      as L


------------------------------------------------------------------------------
-- | A type alias for a basic open addressing hash table using linear
-- probing. See "Data.HashTable.ST.Basic".
type BasicHashTable k v = IOHashTable (B.HashTable) k v

-- | A type alias for the cuckoo hash table. See "Data.HashTable.ST.Cuckoo".
type CuckooHashTable k v = IOHashTable (Cu.HashTable) k v

-- | A type alias for the linear hash table. See "Data.HashTable.ST.Linear".
type LinearHashTable k v = IOHashTable (L.HashTable) k v


------------------------------------------------------------------------------
-- | A type alias for our hash tables, which run in 'ST', to set the state
-- token type to 'PrimState' 'IO' (aka 'RealWorld') so that we can use them in
-- 'IO'.
type IOHashTable tabletype k v = tabletype (PrimState IO) k v


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.new'.
new :: C.HashTable h => IO (IOHashTable h k v)
new = stToIO C.new
{-# INLINE new #-}
{-# SPECIALIZE INLINE new :: IO (BasicHashTable k v) #-}
{-# SPECIALIZE INLINE new :: IO (LinearHashTable k v) #-}
{-# SPECIALIZE INLINE new :: IO (CuckooHashTable k v) #-}

------------------------------------------------------------------------------
-- | See the documentation for this function in
-- 'Data.HashTable.Class.newSized'.
newSized :: C.HashTable h => Int -> IO (IOHashTable h k v)
newSized = stToIO . C.newSized
{-# INLINE newSized #-}
{-# SPECIALIZE INLINE newSized :: Int -> IO (BasicHashTable k v) #-}
{-# SPECIALIZE INLINE newSized :: Int -> IO (LinearHashTable k v) #-}
{-# SPECIALIZE INLINE newSized :: Int -> IO (CuckooHashTable k v) #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.insert'.
insert   :: (C.HashTable h, Eq k, Hashable k) =>
            IOHashTable h k v -> k -> v -> IO ()
insert h k v = stToIO $ C.insert h k v
{-# INLINE insert #-}
{-# SPECIALIZE INLINE insert :: Hashable k =>
                         BasicHashTable  k v -> k -> v -> IO () #-}
{-# SPECIALIZE INLINE insert :: Hashable k =>
                         LinearHashTable k v -> k -> v -> IO () #-}
{-# SPECIALIZE INLINE insert :: Hashable k =>
                         CuckooHashTable k v -> k -> v -> IO () #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.delete'.
delete   :: (C.HashTable h, Eq k, Hashable k) =>
            IOHashTable h k v -> k -> IO ()
delete h k = stToIO $ C.delete h k
{-# INLINE delete #-}
{-# SPECIALIZE INLINE delete :: Hashable k =>
                         BasicHashTable  k v -> k -> IO () #-}
{-# SPECIALIZE INLINE delete :: Hashable k =>
                         LinearHashTable k v -> k -> IO () #-}
{-# SPECIALIZE INLINE delete :: Hashable k =>
                         CuckooHashTable k v -> k -> IO () #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.lookup'.
lookup   :: (C.HashTable h, Eq k, Hashable k) =>
            IOHashTable h k v -> k -> IO (Maybe v)
lookup h k = stToIO $ C.lookup h k
{-# INLINE lookup #-}
{-# SPECIALIZE INLINE lookup :: Hashable k =>
                         BasicHashTable  k v -> k -> IO (Maybe v) #-}
{-# SPECIALIZE INLINE lookup :: Hashable k =>
                         LinearHashTable k v -> k -> IO (Maybe v) #-}
{-# SPECIALIZE INLINE lookup :: Hashable k =>
                         CuckooHashTable k v -> k -> IO (Maybe v) #-}

------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.lookupIndex'.
lookupIndex   :: (C.HashTable h, Eq k, Hashable k) =>
                 IOHashTable h k v -> k -> IO (Maybe Word)
lookupIndex h k = stToIO $ C.lookupIndex h k
{-# INLINE lookupIndex #-}
{-# SPECIALIZE INLINE lookupIndex :: Hashable k =>
                         BasicHashTable  k v -> k -> IO (Maybe Word) #-}
{-# SPECIALIZE INLINE lookupIndex :: Hashable k =>
                         LinearHashTable k v -> k -> IO (Maybe Word) #-}
{-# SPECIALIZE INLINE lookupIndex :: Hashable k =>
                         CuckooHashTable k v -> k -> IO (Maybe Word) #-}

------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.nextByIndex'.
nextByIndex   :: (C.HashTable h, Eq k, Hashable k) =>
                 IOHashTable h k v -> Word -> IO (Maybe (Word,k,v))
nextByIndex h k = stToIO $ C.nextByIndex h k
{-# INLINE nextByIndex #-}
{-# SPECIALIZE INLINE nextByIndex :: Hashable k =>
                         BasicHashTable  k v -> Word -> IO (Maybe (Word,k,v)) #-}
{-# SPECIALIZE INLINE nextByIndex :: Hashable k =>
                         LinearHashTable k v -> Word -> IO (Maybe (Word,k,v)) #-}
{-# SPECIALIZE INLINE nextByIndex :: Hashable k =>
                         CuckooHashTable k v -> Word -> IO (Maybe (Word,k,v)) #-}

------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.mutateST'.
mutateIO   :: (C.HashTable h, Eq k, Hashable k) =>
              IOHashTable h k v -> k -> (Maybe v -> IO (Maybe v, a)) -> IO a
mutateIO h k f = stToIO $ C.mutateST h k (ioToST . f)
{-# INLINE mutateIO #-}
{-# SPECIALIZE INLINE mutateIO :: Hashable k =>
                         BasicHashTable  k v -> k -> (Maybe v -> IO (Maybe v, a)) -> IO a #-}
{-# SPECIALIZE INLINE mutateIO :: Hashable k =>
                         LinearHashTable k v -> k -> (Maybe v -> IO (Maybe v, a)) -> IO a #-}
{-# SPECIALIZE INLINE mutateIO :: Hashable k =>
                         CuckooHashTable k v -> k -> (Maybe v -> IO (Maybe v, a)) -> IO a #-}

------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.mutate'.
mutate   :: (C.HashTable h, Eq k, Hashable k) =>
            IOHashTable h k v -> k -> (Maybe v -> (Maybe v, a)) -> IO a
mutate h k f = stToIO $ C.mutate h k f
{-# INLINE mutate #-}
{-# SPECIALIZE INLINE mutate :: Hashable k =>
                         BasicHashTable  k v -> k -> (Maybe v -> (Maybe v, a)) -> IO a #-}
{-# SPECIALIZE INLINE mutate :: Hashable k =>
                         LinearHashTable k v -> k -> (Maybe v -> (Maybe v, a)) -> IO a #-}
{-# SPECIALIZE INLINE mutate :: Hashable k =>
                         CuckooHashTable k v -> k -> (Maybe v -> (Maybe v, a)) -> IO a #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- 'Data.HashTable.Class.fromList'.
fromList :: (C.HashTable h, Eq k, Hashable k) =>
            [(k,v)] -> IO (IOHashTable h k v)
fromList = stToIO . C.fromList
{-# INLINE fromList #-}
{-# SPECIALIZE INLINE fromList :: Hashable k =>
                           [(k,v)] -> IO (BasicHashTable  k v) #-}
{-# SPECIALIZE INLINE fromList :: Hashable k =>
                           [(k,v)] -> IO (LinearHashTable k v) #-}
{-# SPECIALIZE INLINE fromList :: Hashable k =>
                           [(k,v)] -> IO (CuckooHashTable k v) #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- 'Data.HashTable.Class.fromListWithSizeHint'.
fromListWithSizeHint :: (C.HashTable h, Eq k, Hashable k) =>
                        Int -> [(k,v)] -> IO (IOHashTable h k v)
fromListWithSizeHint n = stToIO . C.fromListWithSizeHint n
{-# INLINE fromListWithSizeHint #-}
{-# SPECIALIZE INLINE fromListWithSizeHint :: Hashable k =>
                           Int -> [(k,v)] -> IO (BasicHashTable  k v) #-}
{-# SPECIALIZE INLINE fromListWithSizeHint :: Hashable k =>
                           Int -> [(k,v)] -> IO (LinearHashTable k v) #-}
{-# SPECIALIZE INLINE fromListWithSizeHint :: Hashable k =>
                           Int -> [(k,v)] -> IO (CuckooHashTable k v) #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.toList'.
toList   :: (C.HashTable h, Eq k, Hashable k) =>
            IOHashTable h k v -> IO [(k,v)]
toList = stToIO . C.toList
{-# INLINE toList #-}
{-# SPECIALIZE INLINE toList :: Hashable k =>
                         BasicHashTable  k v -> IO [(k,v)] #-}
{-# SPECIALIZE INLINE toList :: Hashable k =>
                         LinearHashTable k v -> IO [(k,v)] #-}
{-# SPECIALIZE INLINE toList :: Hashable k =>
                         CuckooHashTable k v -> IO [(k,v)] #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.foldM'.
foldM :: (C.HashTable h) =>
         (a -> (k,v) -> IO a)
      -> a
      -> IOHashTable h k v -> IO a
foldM f seed ht = stToIO $ C.foldM f' seed ht
  where
    f' !i !t = unsafeIOToST $ f i t
{-# INLINE foldM #-}
{-# SPECIALIZE INLINE foldM :: (a -> (k,v) -> IO a) -> a
                            -> BasicHashTable  k v -> IO a #-}
{-# SPECIALIZE INLINE foldM :: (a -> (k,v) -> IO a) -> a
                            -> LinearHashTable k v -> IO a #-}
{-# SPECIALIZE INLINE foldM :: (a -> (k,v) -> IO a) -> a
                            -> CuckooHashTable k v -> IO a #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in 'Data.HashTable.Class.mapM_'.
mapM_ :: (C.HashTable h) => ((k,v) -> IO a) -> IOHashTable h k v -> IO ()
mapM_ f ht = stToIO $ C.mapM_ f' ht
  where
    f' = unsafeIOToST . f
{-# INLINE mapM_ #-}
{-# SPECIALIZE INLINE mapM_ :: ((k,v) -> IO a) -> BasicHashTable  k v
                            -> IO () #-}
{-# SPECIALIZE INLINE mapM_ :: ((k,v) -> IO a) -> LinearHashTable k v
                            -> IO () #-}
{-# SPECIALIZE INLINE mapM_ :: ((k,v) -> IO a) -> CuckooHashTable k v
                            -> IO () #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- 'Data.HashTable.Class.computeOverhead'.
computeOverhead :: (C.HashTable h) => IOHashTable h k v -> IO Double
computeOverhead = stToIO . C.computeOverhead
{-# INLINE computeOverhead #-}
