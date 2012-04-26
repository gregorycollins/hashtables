{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.Hashable
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import qualified Data.HashMap.Strict as UC
import qualified Data.HashTable as H
import qualified Data.Map as Map
import qualified Data.HashTable.IO as IOH
import           Data.Benchmarks.UnorderedCollections.Distributions
import           Data.Benchmarks.UnorderedCollections.Types
import           System.Environment
import           System.FilePath
import           System.Random.MWC

import           Criterion.Collection.Main
import           Criterion.Collection.Sample
import           Criterion.Collection.Types


------------------------------------------------------------------------------
dataMap :: DataStructure (Operation ByteString)
dataMap = setupData Map.empty f
  where
    f !m !op = case op of
                 (Insert k v) -> let !m' = Map.insert k v m in m'
                 (Lookup k)   -> let !_  = Map.lookup k m in m
                 (Delete k)   -> let !m' = Map.delete k m in m'


------------------------------------------------------------------------------
hashMap :: DataStructure (Operation ByteString)
hashMap = setupData UC.empty f
  where
    f !m !op = case op of
                 (Insert k v) -> let !m' = UC.insert k v m in m'
                 (Lookup k)   -> let !_  = UC.lookup k m in m
                 (Delete k)   -> let !m' = UC.delete k m in m'


------------------------------------------------------------------------------
hashTable :: DataStructure (Operation ByteString)
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


------------------------------------------------------------------------------
basicHashTable :: DataStructure (Operation ByteString)
basicHashTable = setupDataIO (IOH.newSized :: Int -> IO (IOH.BasicHashTable k v))
                             f
  where
    f !m !op = case op of
                 (Insert k v) -> IOH.insert m k v >> return m
                 (Lookup k)   -> do
                           !_ <- IOH.lookup m k
                           return m
                 (Delete k)   -> IOH.delete m k >> return m

------------------------------------------------------------------------------

cuckooHashTable :: DataStructure (Operation ByteString)
cuckooHashTable = setupDataIO (IOH.newSized :: Int -> IO (IOH.CuckooHashTable k v))
                             f
  where
    f !m !op = case op of
                 (Insert k v) -> IOH.insert m k v >> return m
                 (Lookup k)   -> do
                           !_ <- IOH.lookup m k
                           return m
                 (Delete k)   -> IOH.delete m k >> return m


------------------------------------------------------------------------------
linearHashTable :: DataStructure (Operation ByteString)
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


mkByteString :: GenIO -> IO ByteString
mkByteString rng = do
    n <- uniformR (4,16) rng
    xs <- replicateM n (uniform rng)
    let !s = B.pack xs
    return $! B16.encode s


instance NFData ByteString where
    rnf s = rnf $! B.unpack s


-- testStructures = [ ("Data.CuckooHashTable" , cuckooHashTable)
--                  ]

-- testStructures = [ ("Data.Map"             , dataMap        )
--                  , ("Data.Hashtable"       , hashTable      )
--                  , ("Data.BasicHashTable"  , basicHashTable )
--                  , ("Data.LinearHashTable" , linearHashTable)
--                  ]

-- testStructures = [ ("Data.BasicHashTable"  , basicHashTable )
--                  ]

testStructures = [ ("Data.Map"             , dataMap        )
                 , ("Data.Hashtable"       , hashTable      )
                 , ("Data.HashMap"         , hashMap        )
                 , ("Data.BasicHashTable"  , basicHashTable )
                 , ("Data.LinearHashTable" , linearHashTable)
                 , ("Data.CuckooHashTable" , cuckooHashTable)
                 ]

-- testStructures = [ ("Data.Hashtable"       , hashTable      )
--                  , ("Data.LinearHashTable" , linearHashTable)
--                  ]


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

-- testSizes = [ 1024000
--             , 2048000
--             ]

-- testSizes = [ 256000
--             , 512000
--             ]

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
main :: IO ()
main = do
    args <- getArgs
    let fn = case args of []    -> Nothing
                          (x:_) -> Just (dropExtensions x)

    let cfg = defaultCriterionCollectionConfig
    runBenchmark PerBatch Mutating insertBenchmark cfg (fmap (++".insert") fn)
    runBenchmark PerBatch Pure lookupBenchmark cfg (fmap (++".lookup") fn)

