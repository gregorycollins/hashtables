{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The criterion collection is a set of utilities for benchmarking data
-- structures using criterion
-- (<http://hackage.haskell.org/package/criterion>).
--
-- The criterion collection allows you to test the /per-operation/ asymptotic
-- performance of your data structures under a variety of simulated
-- workloads. For testing a hash table, for example, you might be interested
-- in:
--
--  * how lookup and insert performance changes as the number of elements in
--    your hash table grows
--
--  * how lookup performance changes depending on the distribution of the
--    lookup keys; you might expect a heavily skewed lookup distribution, where
--    most of the requests are for a small subset of the keys, to have
--    different performance characteristics than a set of lookups for keys
--    uniformly distributed in the keyspace.
--
--  * how the hashtable performs under a mixed workload of inserts, deletes,
--    and lookups.
--
-- Whereas "Criterion" allows you to run a single benchmark a number of times
-- to see how fast it runs, @criterion-collection@ makes performance-testing
-- data structures easier by decoupling benchmarking from workload generation,
-- allowing you to see in-depth how performance changes as the input size
-- varies.
--
-- To test your data structure using @criterion-collection@, you provide the
-- following:
--
-- 1. A datatype which models the set of data structure operations you're
-- interested in testing. For instance, for our hashtable example, your
-- datatype might look like:
--
-- > data Operation k = 
-- >       -- | Insert a k-v pair into the collection. If k existed, we
-- >       --   should update the mapping.
-- >       Insert {-# UNPACK #-} !k
-- >              {-# UNPACK #-} !Int
-- >       -- | Lookup a key in the mapping.
-- >     | Lookup {-# UNPACK #-} !k
-- >       -- | Delete a key from the mapping.
-- >     | Delete {-# UNPACK #-} !k
-- >   deriving (Show)
-- > 
-- > 
-- > instance (NFData k) => NFData (Operation k) where
-- >     rnf (Insert k v) = rnf k `seq` rnf v
-- >     rnf (Lookup k)   = rnf k
-- >     rnf (Delete k)   = rnf k
--
-- 2. A function which, given an operation, runs it on your datastructure.
--
-- 3. A \"ground state\" for your datastructure, usually \"empty\". You can
-- test both pure data structures and data structures in 'IO'.
--
-- 4. One or more \"workload simulators\" which, given a random number
-- generator and an input size, give you back some functions to generate
-- workloads:
--
--   a) to prepopulate the data structure prior to the test
--
--   b) to test the data structure with.
--
-- (Side note: the reason @criterion-collection@ asks you to reify the
-- operation type instead of just generating a list of mutation functions of
-- type @[m -> m]@ is so you can test multiple datastructures under the same
-- workload.)


module Criterion.Collection.Types
  ( Workload(..)
  , WorkloadGenerator
  , WorkloadMonad
  , runWorkloadMonad
  , getRNG
  , DataStructure
  , emptyData
  , runOperation
  , setupData
  , setupDataIO
  ) where

------------------------------------------------------------------------------
import           Criterion.Collection.Internal.Types
