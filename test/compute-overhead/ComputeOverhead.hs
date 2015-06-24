{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.HashTable.Class                 as C
import           Data.HashTable.IO
import           Data.HashTable.Test.Common
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Statistics.Quantile (continuousBy, cadpw)
import           Statistics.Sample
import           System.Environment
import           System.Random.MWC


overhead :: C.HashTable h =>
            FixedTableType h ->
            GenIO ->
            IO Double
overhead dummy rng = do
    size <- uniformR (1000,50000) rng
    !v <- replicateM' size $ uniform rng
    let _ = v :: [(Int,Int)]

    !ht <- fromList v
    forceType dummy ht

    x <- computeOverhead ht
    return x

  where
    replicateM' :: Int -> IO a -> IO [a]
    replicateM' !sz f = go sz []
      where
        go !i !l | i == 0 = return l
                 | otherwise = do
                     !x <- f
                     go (i-1) (x:l)


-- Returns mean / stddev
runTrials :: C.HashTable h =>
             FixedTableType h
          -> GenIO
          -> Int
          -> IO (Double, Double, Double, Double)
runTrials dummy rng ntrials = do
    sample <- rep ntrials $ overhead dummy rng

    let (m, v) = meanVarianceUnb sample
    return (m, sqrt v, p95 sample, pMax sample)
  where
    p95 sample = continuousBy cadpw 19 20 sample

    pMax sample = V.foldl' max (-1) sample

    rep !n !f = do
        mv <- VM.new n
        go mv

      where
        go !mv = go' 0
          where
            go' !i | i >= n = V.unsafeFreeze mv
                   | otherwise = do
                !d <- f
                VM.unsafeWrite mv i d
                go' $ i+1
        

main :: IO ()
main = do
    rng <- do
        args <- getArgs
        if null args
          then withSystemRandom (\x -> (return x) :: IO GenIO)
          else initialize $ V.fromList [read $ head args]

    runTrials dummyLinearTable rng nTrials >>= report "linear hash table"
    runTrials dummyBasicTable rng nTrials >>= report "basic hash table"
    runTrials dummyCuckooTable rng nTrials >>= report "cuckoo hash table"

  where
    nTrials = 200

    report name md = putStrLn msg
      where msg = concat [ "\n(Mean,StdDev,95%,Max) for overhead of "
                         , name
                         , " ("
                         , show nTrials
                         , " trials): "
                         , show md
                         , "\n" ]

    dummyBasicTable = dummyTable
                      :: forall k v . BasicHashTable k v

    dummyLinearTable = dummyTable
                       :: forall k v . LinearHashTable k v

    dummyCuckooTable = dummyTable
                       :: forall k v . CuckooHashTable k v
    
