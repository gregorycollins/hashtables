{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans
import           Data.Benchmarks.UnorderedCollections.Distributions
import           Data.Benchmarks.UnorderedCollections.Types
import           Data.Bits
import           Data.ByteString                                    (ByteString)
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Base16                             as B16
import           Data.Hashable
import qualified Data.HashMap.Strict                                as UC
import qualified Data.HashTable                                     as H
import qualified Data.HashTable.IO                                  as IOH
import           Data.IORef
import qualified Data.Map                                           as Map
import           System.Environment
import           System.FilePath
import           System.Random.MWC

import           Criterion.Collection.Main
import           Criterion.Collection.Sample
import           Criterion.Collection.Types


------------------------------------------------------------------------------
#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString
#endif

------------------------------------------------------------------------------
dataMap :: (Ord k, Eq k) => DataStructure (Operation k)
dataMap = setupData Map.empty f
  where
    f !m !op = case op of
                 (Insert k v) -> let !m' = Map.insert k v m in m'
                 (Lookup k)   -> let !_  = Map.lookup k m in m
                 (Delete k)   -> let !m' = Map.delete k m in m'
{-# INLINE dataMap #-}


------------------------------------------------------------------------------
hashMap :: (Hashable k, Eq k) => DataStructure (Operation k)
hashMap = setupData UC.empty f
  where
    f !m !op = case op of
                 (Insert k v) -> let !m' = UC.insert k v m in m'
                 (Lookup k)   -> let !_  = UC.lookup k m in m
                 (Delete k)   -> let !m' = UC.delete k m in m'
{-# INLINE hashMap #-}


------------------------------------------------------------------------------
hashTable :: (Hashable k, Eq k) => DataStructure (Operation k)
hashTable = setupDataIO (const (H.new (==) (toEnum . (.&. 0x7fffffff) . hash))) f
  where
    f !m !op = case op of
                 (Insert k v) -> H.update m k v >> return m
                 (Lookup k)   -> do
                         !_ <- H.lookup m k
                         return m
                 (Delete k)   -> do
                         !_ <- H.delete m k
                         return m
{-# INLINE hashTable #-}


------------------------------------------------------------------------------
basicHashTable :: (Hashable k, Eq k) => DataStructure (Operation k)
basicHashTable = setupDataIO (IOH.newSized :: Int -> IO (IOH.BasicHashTable k v))
                             f
  where
    f !m !op = case op of
                 (Insert k v) -> IOH.insert m k v >> return m
                 (Lookup k)   -> do
                           !_ <- IOH.lookup m k
                           return m
                 (Delete k)   -> IOH.delete m k >> return m
{-# INLINE basicHashTable #-}


------------------------------------------------------------------------------
cuckooHashTable :: (Hashable k, Eq k) => DataStructure (Operation k)
cuckooHashTable = setupDataIO (IOH.newSized :: Int -> IO (IOH.CuckooHashTable k v))
                             f
  where
    f !m !op = case op of
                 (Insert k v) -> IOH.insert m k v >> return m
                 (Lookup k)   -> do
                           !_ <- IOH.lookup m k
                           return m
                 (Delete k)   -> IOH.delete m k >> return m
{-# INLINE cuckooHashTable #-}


------------------------------------------------------------------------------
linearHashTable :: (Hashable k, Eq k) => DataStructure (Operation k)
linearHashTable = setupDataIO
                    (IOH.newSized :: Int -> IO (IOH.LinearHashTable k v))
                    f
  where
    f !m !op = case op of
                 (Insert k v) -> IOH.insert m k v >> return m
                 (Lookup k)   -> do
                           !_ <- IOH.lookup m k
                           return m
                 (Delete k)   -> IOH.delete m k >> return m
{-# INLINE linearHashTable #-}


------------------------------------------------------------------------------
mkByteString :: GenIO -> IO ByteString
mkByteString rng = do
    n <- uniformR (4,16) rng
    xs <- replicateM n (uniform rng)
    let !s = B.pack xs
    return $! B16.encode s


------------------------------------------------------------------------------
mkConsecutiveIntegers :: IORef Int -> GenIO -> IO Int
mkConsecutiveIntegers ref _ = do
    !i <- atomicModifyIORef ref f
    return $! i
  where
    f !i = let !j = i+1 in (j,j)


------------------------------------------------------------------------------
newtype IntMix = IntMix Int
  deriving (Num, Read, Show, Ord, Eq, NFData)


------------------------------------------------------------------------------
instance Hashable IntMix where
    hash (IntMix a) = hashWithSalt 1102951999 a
    hashWithSalt salt (IntMix a) = hashWithSalt salt a


------------------------------------------------------------------------------
loadConsecutiveIntegersWorkload :: WorkloadGenerator (Operation Int)
loadConsecutiveIntegersWorkload size = do
    ref <- liftIO $ newIORef 0
    loadOnly (mkConsecutiveIntegers ref) size


------------------------------------------------------------------------------
loadConsecutiveIntegersWorkload' :: WorkloadGenerator (Operation IntMix)
loadConsecutiveIntegersWorkload' size = do
    ref <- liftIO $ newIORef 0
    loadOnly (\rng -> IntMix `fmap` mkConsecutiveIntegers ref rng) size


------------------------------------------------------------------------------
testStructures = [ ("Data.Map"             , dataMap        )
                 , ("Data.Hashtable"       , hashTable      )
                 , ("Data.HashMap"         , hashMap        )
                 , ("Data.BasicHashTable"  , basicHashTable )
                 , ("Data.LinearHashTable" , linearHashTable)
                 , ("Data.CuckooHashTable" , cuckooHashTable)
                 ]

intStructures = [ ("Data.Map"            , dataMap        )
                , ("Data.Hashtable"      , hashTable      )
                , ("Data.HashMap"        , hashMap        )
                , ("Data.BasicHashTable" , basicHashTable )
                , ("Data.CuckooHashTable", cuckooHashTable)
                ]

intStructures' = [ ("Data.Map"            , dataMap        )
                 , ("Data.Hashtable"      , hashTable      )
                 , ("Data.HashMap"        , hashMap        )
                 , ("Data.BasicHashTable" , basicHashTable )
                 , ("Data.CuckooHashTable", cuckooHashTable)
                 ]


------------------------------------------------------------------------------
testSizes :: [Int]
testSizes = [ 250
            , 500
            , 1000
            , 2000
            , 4000
            , 8000
            , 16000
            , 32000
            , 64000
            , 128000
            , 256000
            , 512000
            , 1024000
            , 2048000 ]


------------------------------------------------------------------------------
lookupBenchmark :: Benchmark (Operation ByteString)
lookupBenchmark = Benchmark "Lookup Performance"
                      testStructures
                      testSizes
                      (loadAndUniformLookup mkByteString)


------------------------------------------------------------------------------
insertBenchmark :: Benchmark (Operation ByteString)
insertBenchmark = Benchmark "Insert Performance"
                      testStructures
                      testSizes
                      (loadOnly mkByteString)


------------------------------------------------------------------------------
consecutiveIntBenchmark :: Benchmark (Operation Int)
consecutiveIntBenchmark = Benchmark "Insert consecutive ints"
                              intStructures
                              testSizes
                              loadConsecutiveIntegersWorkload


------------------------------------------------------------------------------
consecutiveIntWithMixBenchmark :: Benchmark (Operation IntMix)
consecutiveIntWithMixBenchmark = Benchmark "Insert consecutive ints (mixed)"
                                     intStructures'
                                     testSizes
                                     loadConsecutiveIntegersWorkload'

------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let fn = case args of []    -> Nothing
                          (x:_) -> Just (dropExtensions x)

    let cfg = defaultCriterionCollectionConfig
    runBenchmark PerBatch Mutating insertBenchmark cfg (fmap (++".insert") fn)
    runBenchmark PerBatch Pure lookupBenchmark cfg (fmap (++".lookup") fn)
    runBenchmark PerBatch Mutating consecutiveIntBenchmark cfg
                 (fmap (++".int") fn)
    runBenchmark PerBatch Mutating consecutiveIntWithMixBenchmark cfg
                 (fmap (++".intmix") fn)

