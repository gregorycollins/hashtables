{-# LANGUAGE RankNTypes #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Data.HashTable.Test.Common as Common
import qualified Data.HashTable.ST.Basic as B
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.ST.Linear as L
import qualified Data.HashTable.IO as IO


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "All" tests
  where
    dummyBasicTable = Common.dummyTable
                      :: forall k v . IO.IOHashTable (B.HashTable) k v

    dummyCuckooTable = Common.dummyTable
                      :: forall k v . IO.IOHashTable (C.HashTable) k v

    dummyLinearTable = Common.dummyTable
                      :: forall k v . IO.IOHashTable (L.HashTable) k v


    basicTests  = Common.tests "basic" dummyBasicTable
    cuckooTests = Common.tests "cuckoo" dummyCuckooTable
    linearTests = Common.tests "linear" dummyLinearTable

    tests = [basicTests, linearTests, cuckooTests]
