{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Criterion.Collection.Internal.Types
  ( Workload(..)
  , WorkloadGenerator
  , WorkloadMonad(..)
  , runWorkloadMonad
  , getRNG
  , DataStructure(..)
  , setupData
  , setupDataIO
  ) where

------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Monad.Reader
import           Data.Vector (Vector)
import           System.Random.MWC

------------------------------------------------------------------------------
-- Some thoughts on benchmarking modes
--
-- * pre-fill data structure, test an operation workload without modifying the
--   data structure, measure time for each operation
--
--   ---> allows you to get fine-grained per-operation times with distributions
--
-- * pre-fill data structure, get a bunch of work to do (cumulatively modifying
--   the data structure), measure time per-operation OR for the whole batch and
--   divide out
--
--
-- Maybe it will look like this?
-- > data MeasurementMode = PerBatch | PerOperation
-- > data WorkloadMode = Pure | Mutating

------------------------------------------------------------------------------
newtype WorkloadMonad a = WM (ReaderT GenIO IO a)
  deriving (Monad, MonadIO)


------------------------------------------------------------------------------
runWorkloadMonad :: WorkloadMonad a -> GenIO -> IO a
runWorkloadMonad (WM m) gen = runReaderT m gen


------------------------------------------------------------------------------
getRNG :: WorkloadMonad GenIO
getRNG = WM ask


------------------------------------------------------------------------------
-- | Given an 'Int' representing \"input size\", a 'WorkloadGenerator' makes a
-- 'Workload'. @Workload@s generate operations to prepopulate data structures
-- with /O(n)/ data items, then generate operations on-demand to benchmark your
-- data structure according to some interesting distribution.
type WorkloadGenerator op = Int -> WorkloadMonad (Workload op)


------------------------------------------------------------------------------
data (NFData op) => Workload op = Workload {
      -- | \"Setup work\" is work that you do to prepopulate a data structure
      -- to a certain size before testing begins.
      setupWork             :: !(Vector op)

      -- | Given the number of operations to produce, 'genWorkload' spits out a
      -- randomly-distributed workload simulation to be used in the benchmark.
      --
      -- | Some kinds of skewed workload distributions (the canonical example
      -- being \"frequent lookups for a small set of keys and infrequent
      -- lookups for the others\") need a certain minimum number of operations
      -- to be generated to be statistically valid, which only the
      -- 'WorkloadGenerator' would know how to decide. In these cases, you are
      -- free to return more than @N@ samples from 'genWorkload', and
      -- @criterion-collection@ will run them all for you.
      --
      -- Otherwise, @criterion-collection@ is free to bootstrap your benchmark
      -- using as many sample points as it would take to make the results
      -- statistically relevant.
    , genWorkload           :: !(Int -> WorkloadMonad (Vector op))
}


------------------------------------------------------------------------------
data DataStructure op = forall m . DataStructure {
      emptyData    :: !(Int -> IO m)
    , runOperation :: !(m -> op -> IO m)
}


------------------------------------------------------------------------------
setupData :: m -> (m -> op -> m) -> DataStructure op
setupData e r = DataStructure (const $ return e) (\m o -> return $ r m o)


------------------------------------------------------------------------------
setupDataIO :: (Int -> IO m) -> (m -> op -> IO m) -> DataStructure op
setupDataIO = DataStructure
