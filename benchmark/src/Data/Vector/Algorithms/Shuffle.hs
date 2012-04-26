{-# LANGUAGE BangPatterns #-}
module Data.Vector.Algorithms.Shuffle
  ( shuffle ) where

import           Control.Monad.ST             (unsafeIOToST)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as MV
import           System.Random.MWC


shuffle :: GenIO -> Vector k -> Vector k
shuffle rng v = V.modify go v
  where
    !n = V.length v

    go mv = f (n-1)
      where
        -- note: inclusive
        pick b = unsafeIOToST $ uniformR (0,b) rng

        swap = MV.unsafeSwap mv

        f 0  = return ()
        f !k = do
            idx <- pick k
            swap k idx
            f (k-1)
            
