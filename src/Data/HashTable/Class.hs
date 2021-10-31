{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

-- | This module contains a 'HashTable' typeclass for the hash table
-- implementations in this package. This allows you to provide functions which
-- will work for any hash table implementation in this collection.
--
-- It is recommended to create a concrete type alias in your code when using this
-- package, i.e.:
--
-- > import qualified Data.HashTable.IO as H
-- >
-- > type HashTable k v = H.BasicHashTable k v
-- >
-- > foo :: IO (HashTable Int Int)
-- > foo = do
-- >     ht <- H.new
-- >     H.insert ht 1 1
-- >     return ht
--
-- or
--
-- > import qualified Data.HashTable.ST.Cuckoo as C
-- > import qualified Data.HashTable.Class as H
-- >
-- > type HashTable s k v = C.HashTable s k v
-- >
-- > foo :: ST s (HashTable s k v)
-- > foo = do
-- >     ht <- H.new
-- >     H.insert ht 1 1
-- >     return ht
--
-- Firstly, this makes it easy to switch to a different hash table
-- implementation, and secondly, using a concrete type rather than leaving your
-- functions abstract in the 'HashTable' class should allow GHC to optimize
-- away the typeclass dictionaries.
--
-- Note that the functions in this typeclass are in the 'ST' monad; if you want
-- hash tables in 'IO', use the convenience wrappers in "Data.HashTable.IO".
--
module Data.HashTable.Class
  ( HashTable(..)
  , fromList
  , fromListWithSizeHint
  , toList
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
import           Data.Word           (Word)
#endif
import           Control.Monad.ST
import           Data.Hashable
import           Prelude             hiding (mapM_)

-- | A typeclass for hash tables in the 'ST' monad. The operations on these
-- hash tables are typically both key- and value-strict.
class HashTable h where
    -- | Creates a new, default-sized hash table. /O(1)/.
    new      :: ST s (h s k v)

    -- | Creates a new hash table sized to hold @n@ elements. /O(n)/.
    newSized :: Int -> ST s (h s k v)

    -- | Generalized update. Given a key /k/, and a user function /f/, calls:
    --
    --   - `f Nothing` if the key did not exist in the hash table
    --   - `f (Just v)` otherwise
    --
    -- If the user function returns @(Nothing, _)@, then the value is deleted
    -- from the hash table. Otherwise the mapping for /k/ is inserted or
    -- replaced with the provided value.
    --
    -- Returns the second part of the tuple returned by /f/.
    mutate :: (Eq k, Hashable k) =>
              h s k v -> k -> (Maybe v -> (Maybe v, a)) -> ST s a
    mutate tbl k f = mutateST tbl k (pure . f)

    -- | As 'mutate', except that the action can perform additional side
    -- effects.
    mutateST :: (Eq k, Hashable k) =>
                h s k v -> k -> (Maybe v -> ST s (Maybe v, a)) -> ST s a

    -- | Inserts a key/value mapping into a hash table, replacing any existing
    -- mapping for that key.
    --
    -- /O(n)/ worst case, /O(1)/ amortized.
    insert   :: (Eq k, Hashable k) => h s k v -> k -> v -> ST s ()

    -- | Deletes a key-value mapping from a hash table. /O(n)/ worst case,
    -- /O(1)/ amortized.
    delete   :: (Eq k, Hashable k) => h s k v -> k -> ST s ()

    -- | Looks up a key-value mapping in a hash table. /O(n)/ worst case,
    -- (/O(1)/ for cuckoo hash), /O(1)/ amortized.
    lookup   :: (Eq k, Hashable k) => h s k v -> k -> ST s (Maybe v)

    -- | A strict fold over the key-value records of a hash table in the 'ST'
    -- monad. /O(n)/.
    foldM    :: (a -> (k,v) -> ST s a) -> a -> h s k v -> ST s a

    -- | A side-effecting map over the key-value records of a hash
    -- table. /O(n)/.
    mapM_    :: ((k,v) -> ST s b) -> h s k v -> ST s ()

    -- | Looks up the index of a key-value mapping in a hash table suitable
    -- for passing to 'nextByIndex'.
    lookupIndex :: (Eq k, Hashable k) => h s k v -> k -> ST s (Maybe Word)

    -- | Returns the next key-value mapping stored at the given index or at
    -- a greater index. The index, key, and value of the next record are
    -- returned.
    nextByIndex :: h s k v -> Word -> ST s (Maybe (Word,k,v))

    -- | Computes the overhead (in words) per key-value mapping. Used for
    -- debugging, etc; time complexity depends on the underlying hash table
    -- implementation. /O(n)/.
    computeOverhead :: h s k v -> ST s Double


------------------------------------------------------------------------------
-- | Create a hash table from a list of key-value pairs. /O(n)/.
fromList :: (HashTable h, Eq k, Hashable k) => [(k,v)] -> ST s (h s k v)
fromList l = do
    ht <- new
    go ht l

  where
    go ht = go'
      where
        go' [] = return ht
        go' ((!k,!v):xs) = do
            insert ht k v
            go' xs
{-# INLINE fromList #-}


------------------------------------------------------------------------------
-- | Create a hash table from a list of key-value pairs, with a size hint. /O(n)/.
fromListWithSizeHint :: (HashTable h, Eq k, Hashable k) =>
                        Int
                     -> [(k,v)]
                     -> ST s (h s k v)
fromListWithSizeHint n l = do
    ht <- newSized n
    go ht l

  where
    go ht = go'
      where
        go' [] = return ht
        go' ((!k,!v):xs) = do
            insert ht k v
            go' xs
{-# INLINE fromListWithSizeHint #-}


------------------------------------------------------------------------------
-- | Given a hash table, produce a list of key-value pairs. /O(n)/.
toList :: (HashTable h) => h s k v -> ST s [(k,v)]
toList ht = do
    l <- foldM f [] ht
    return l

  where
    f !l !t = return (t:l)
{-# INLINE toList #-}
