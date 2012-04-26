{-# LANGUAGE BangPatterns #-}

module Data.Benchmarks.UnorderedCollections.Distributions
  ( makeRandomData
  , makeRandomVariateData
    -- * Workloads
  , insertWorkload
  , deleteWorkload
  , uniformLookupWorkload
  , exponentialLookupWorkload

  , loadOnly
  , loadAndUniformLookup
  , loadAndSkewedLookup
  , loadAndDeleteAll
  , loadAndDeleteSome
  , uniformlyMixed
  ) where


import qualified Control.Concurrent.Thread as Th
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans (liftIO)
import           Data.Benchmarks.UnorderedCollections.Types
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import           Data.Vector (Vector)
import qualified Data.Vector.Algorithms.Shuffle as V
import           GHC.Conc (numCapabilities)
import           Statistics.Distribution
import           Statistics.Distribution.Exponential
import           System.Random.MWC

import           Criterion.Collection.Types


------------------------------------------------------------------------------
debug :: (MonadIO m) => String -> m ()
debug s = liftIO $ putStrLn s


------------------------------------------------------------------------------
makeRandomData :: (NFData k) =>
                  (GenIO -> IO k)
               -> Int
               -> WorkloadMonad (Vector (k,Int))
makeRandomData !genFunc !n = do
    rng <- getRNG
    debug $ "making " ++ show n ++ " data items"
    keys <- liftIO $ vreplicateM n rng genFunc
    let !v = keys `V.zip` vals
    let !_ = forceVector v
    debug $ "made " ++ show n ++ " data items"
    return $! v

  where
    vals      = V.enumFromN 0 n


------------------------------------------------------------------------------
makeRandomVariateData :: (Ord k, NFData k, Variate k) =>
                         Int
                      -> WorkloadMonad (Vector (k,Int))
makeRandomVariateData = makeRandomData uniform


------------------------------------------------------------------------------
insertWorkload :: (NFData k) => Vector (k,Int) -> Vector (Operation k)
insertWorkload = mapForce $ \(k,v) -> Insert k v


------------------------------------------------------------------------------
deleteWorkload :: (NFData k) => Vector (k,Int) -> Vector (Operation k)
deleteWorkload = mapForce $ \(k,_) -> Delete k


------------------------------------------------------------------------------
uniformLookupWorkload :: (NFData k) =>
                         Vector (k,Int)
                      -> Int
                      -> WorkloadMonad (Vector (Operation k))
uniformLookupWorkload !vec !ntimes = do
    rng <- getRNG
    debug $ "uniformLookupWorkload: generating " ++ show ntimes ++ " lookups"
    v <- liftIO $ vreplicateM ntimes rng f
    debug $ "uniformLookupWorkload: done"
    return v

  where
    !n = V.length vec
    f r = do
        idx <- pick
        let (k,_) = V.unsafeIndex vec idx
        return $ Lookup k
      where
        pick = uniformR (0,n-1) r


------------------------------------------------------------------------------
exponentialLookupWorkload :: (NFData k) =>
                             Double
                          -> Vector (k,Int)
                          -> Int
                          -> WorkloadMonad (Vector (Operation k))
exponentialLookupWorkload !lambda !vec !ntimes = do
    rng <- getRNG
    liftIO $ vreplicateM ntimes rng f
  where
    !dist = exponential lambda
    !n    = V.length vec
    !n1   = n-1
    !nd   = fromIntegral n

    f r = do
        x <- uniformR (0.1, 7.0) r
        let idx = max 0 . min n1 . round $ nd * density dist x
        let (k,_) = V.unsafeIndex vec idx
        return $! Lookup k


------------------------------------------------------------------------------
loadOnly :: (NFData k) =>
            (GenIO -> IO k)     -- ^ rng for keys
         -> WorkloadGenerator (Operation k)
loadOnly !genFunc !n = return $ Workload V.empty f
  where
    f _ = liftM insertWorkload $ makeRandomData genFunc n


------------------------------------------------------------------------------
loadAndUniformLookup :: (NFData k) =>
                        (GenIO -> IO k)  -- ^ rng for keys
                     -> WorkloadGenerator (Operation k)
loadAndUniformLookup !genFunc !n = do
    !vals <- makeRandomData genFunc n
    let !inserts = insertWorkload vals

    return $! Workload inserts $ uniformLookupWorkload vals


------------------------------------------------------------------------------
loadAndSkewedLookup :: (NFData k) =>
                       (GenIO -> IO k)  -- ^ rng for keys
                    -> WorkloadGenerator (Operation k)
loadAndSkewedLookup !genFunc !n = do
    !vals <- makeRandomData genFunc n
    let !inserts = insertWorkload vals
    return $! Workload inserts $ exponentialLookupWorkload 1.5 vals


------------------------------------------------------------------------------
loadAndDeleteAll :: (NFData k) =>
                    (GenIO -> IO k)     -- ^ key generator
                 -> WorkloadGenerator (Operation k)
loadAndDeleteAll !genFunc !n = do
    rng <- getRNG
    !vals <- makeRandomData genFunc n
    let !inserts = insertWorkload vals
    let !deletes = deleteWorkload $ V.shuffle rng vals

    return $ Workload inserts (const $ return deletes)


------------------------------------------------------------------------------
loadAndDeleteSome :: (NFData k) =>
                     (GenIO -> IO k)
                  -> WorkloadGenerator (Operation k)
loadAndDeleteSome !genFunc !n = do
    !vals <- makeRandomData genFunc n
    let !inserts = insertWorkload vals

    return $ Workload inserts $ f vals

  where
    f vals k = do
        rng <- getRNG
        return $ deleteWorkload $ V.take k $ V.shuffle rng vals


------------------------------------------------------------------------------
uniformlyMixed :: (NFData k) =>
                  (GenIO -> IO k)
               -> Double
               -> Double
               -> WorkloadGenerator (Operation k)
uniformlyMixed !genFunc !lookupPercentage !deletePercentage !n = do
    let !numLookups = ceiling (fromIntegral n * lookupPercentage)
    let !numDeletes = ceiling (fromIntegral n * deletePercentage)

    !vals <- makeRandomData genFunc n
    let !inserts = insertWorkload vals
    !lookups <- uniformLookupWorkload vals numLookups

    rng <- getRNG
    let !deletes = deleteWorkload $ V.take numDeletes $ V.shuffle rng vals
    let !out = V.shuffle rng $ V.concat [inserts, lookups, deletes]

    return $! Workload V.empty $ const $ return $ forceVector out


------------------------------------------------------------------------------
-- utilities
------------------------------------------------------------------------------
forceVector :: (NFData k) => Vector k -> Vector k
forceVector !vec = V.foldl' force () vec `seq` vec
  where
    force x v = x `deepseq` v `deepseq` ()


mapForce :: (NFData b) => (a -> b) -> Vector a -> Vector b
mapForce !f !vIn = let !vOut = V.map f vIn
                   in forceVector vOut


-- split a GenIO
splitGenIO :: GenIO -> IO GenIO
splitGenIO rng = VU.replicateM 256 (uniform rng) >>= initialize


-- vector replicateM is slow as dogshit.
vreplicateM :: Int -> GenIO -> (GenIO -> IO a) -> IO (Vector a)
vreplicateM n origRng act = do
    rngs <- replicateM numCapabilities (splitGenIO origRng)
    mv <- MV.new n
    let actions = map (f mv) (parts `zip` rngs)
    results <- liftM (map snd) $ mapM Th.forkIO actions
    _ <- sequence results
    V.unsafeFreeze mv

  where
    parts = partition (n-1) numCapabilities

    f mv ((low,high),rng) = do
        f' low
      where
        f' !idx | idx > high = return ()
                | otherwise = do
                                x <- act rng
                                MV.unsafeWrite mv idx x
                                f' (idx+1)


partition :: Int -> Int -> [(Int,Int)]
partition n k = ys `zip` xs
  where
    xs = map f [1..k]
    ys = 0:(map (+1) xs)
    f i = (i * n) `div` k
