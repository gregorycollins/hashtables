{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Criterion.Collection.Sample
  ( Benchmark(..)
  , SampleData(..)
  , MeasurementMode(..)
  , WorkloadMode(..)
  , computeMeanAndStddev
  , compute95thPercentile
  , computeMax
  , takeSample
  , takeSamples
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans
import           Criterion hiding (Benchmark)
import           Criterion.Config
import           Criterion.Environment
import           Criterion.IO
import           Criterion.Measurement
import           Criterion.Monad
import           Criterion.Collection.Internal.Types
import           Data.IORef
import           Data.List (foldl')
import           Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Statistics.Quantile (continuousBy, cadpw)
import           Statistics.Sample
import           System.Mem (performGC)
import           System.Random.MWC
import           Text.Printf (printf)

------------------------------------------------------------------------------
data MeasurementMode = PerBatch | PerOperation
data WorkloadMode = Pure | Mutating


------------------------------------------------------------------------------
data SampleData = SampleData {
      sdInputSize :: !Int       -- ^ what was the size of the input for this
                                -- sample?
    , sdNumOps    :: !Int       -- ^ how many operations are covered by this
                                -- sample? For a per-operation measurement,
                                -- this value would be \"1\", and for a batch
                                -- measurement this value would be the number
                                -- of items in the batch.
    , sdData      :: !Sample    -- ^ sample data.
    }


instance Show SampleData where
    show (SampleData is nop da) = "<SampleData inputSize=" ++ show is
                                  ++ ", nops=" ++ show nop
                                  ++ ", sample size=" ++ show (U.length da)
                                  ++ ">"

------------------------------------------------------------------------------
data Benchmark op = Benchmark {
      benchmarkName     :: String
    , dataStructures    :: [(String, DataStructure op)]
    , inputSizes        :: [Int]
    , workloadGenerator :: WorkloadGenerator op
}


------------------------------------------------------------------------------
-- | Given some sample data, compute the mean time per operation (in seconds)
-- and standard deviation
computeMeanAndStddev :: SampleData -> (Double, Double)
computeMeanAndStddev (SampleData _ nops sample) = (v,s)
  where
    nopsReal         = fromIntegral nops
    (meanValue, var) = meanVarianceUnb sample
    stddev           = sqrt $ abs var
    !v               = meanValue / nopsReal
    !s               = stddev / nopsReal


------------------------------------------------------------------------------
-- | Given some sample data, compute the 95th percentile.
compute95thPercentile :: SampleData -> Double
compute95thPercentile (SampleData _ nops sample) = v
  where
    nopsReal         = fromIntegral nops
    quantile         = continuousBy cadpw 19 20 sample
    v                = quantile / nopsReal


------------------------------------------------------------------------------
-- | Given some sample data, compute the maximum value
computeMax :: SampleData -> Double
computeMax (SampleData _ nops sample) = v
  where
    nopsReal         = fromIntegral nops
    maxval           = U.foldl' max 0 sample
    v                = maxval / nopsReal


------------------------------------------------------------------------------
takeSample :: (NFData op) =>
              MeasurementMode
           -> WorkloadMode
           -> Benchmark op
           -> Environment
           -> GenIO
           -> Int
           -> Criterion [SampleData]
takeSample !mMode !wMode !benchmark !env !rng !inputSize = do
    workload <- liftIO $ runWorkloadMonad (workGen inputSize) rng
    let setupOperations = setupWork workload
    let genWorkData     = genWorkload workload

    case mMode of
      PerBatch     -> batch setupOperations genWorkData
      PerOperation -> perOp setupOperations genWorkData


  where
    --------------------------------------------------------------------------
    dss       = dataStructures benchmark
    workGen   = workloadGenerator benchmark

    --------------------------------------------------------------------------
    batch setupOperations genWorkData = do
        workData <- liftIO $ runWorkloadMonad (genWorkData $ inputSize `div` 2)
                                              rng
        let nOps = V.length workData
        mapM (batchOne setupOperations workData nOps) dss


    --------------------------------------------------------------------------
    mkRunOp runOpMutating =
        let runOpPure = \m op -> do
                            m' <- runOpMutating m op
                            return $! m' `seq` m
        in case wMode of
             Pure     -> runOpPure
             Mutating -> runOpMutating


    --------------------------------------------------------------------------
    runWorkData workData chunkSize runOp start i val = go i val
      where
        go !i !val
            | i >= chunkSize = return val
            | otherwise = do
                  let op = V.unsafeIndex workData (start+i)
                  !val' <- runOp val op
                  go (i+1) val'


    --------------------------------------------------------------------------
    batchOne setupOperations workData nOps
             (name, (DataStructure emptyValue runOpMutating)) = do
        note $ "running batch benchmark on " ++ name ++ "\n"
        let minTime = envClockResolution env * 1000
        cfg <- getConfig
        let proc = V.foldM' runOpMutating
        let mkStartValue = emptyValue inputSize >>= flip proc setupOperations
        startValue1 <- liftIO mkStartValue

        liftIO performGC
        let tProc = runWorkData workData nOps runOpMutating 0 0

        prolix $ "running test batch with " ++ show nOps
                 ++ " work items\n"

        (tm,_) <- liftIO $ time (tProc startValue1)

        prolix $ "running initial timing on " ++ show nOps
                 ++ " work items\n"

        let iters = max 5 (ceiling (minTime / tm))

        prolix $ "running benchmark on " ++ show nOps
                 ++ " work items, " ++ show iters ++ " iterations\n"

        sample <- liftIO $ U.generateM iters $ \_ -> do
                      sv <- mkStartValue
                      performGC
                      (!tm,_) <- time (tProc sv)
                      return $ tm - clockCost

        prolix $ "finished batch benchmark on " ++ name ++ "\n"
        return (SampleData inputSize nOps sample)


    --------------------------------------------------------------------------
    perOp setupOperations genWorkData = do
        -- FIXME: lifted this code from criterion, is there some way to merge
        -- them?
        _ <- prolix "generating seed workload"
        seedData <- liftIO $ runWorkloadMonad (genWorkData 1000) rng
        _ <- prolix "seed workload generated"

        workData <- liftIO $ runWorkloadMonad (genWorkData inputSize) rng
        mapM (perOpOne setupOperations workData seedData) dss


    --------------------------------------------------------------------------
    perOpOne setupOperations workData seedData
             (name, (DataStructure emptyValue runOpMutating)) = do
        let runOp = mkRunOp runOpMutating
        let proc = V.foldM' runOpMutating

        note $ "running per-op benchmark on " ++ name ++ "\n"

        startValue <- liftIO (emptyValue inputSize >>=
                              flip proc setupOperations)
        liftIO performGC

        -- warm up clock
        _ <- liftIO $ runForAtLeast 0.1 10000 (`replicateM_` getTime)
        let minTime = envClockResolution env * 1000

        (testTime, testIters, startValue') <-
            liftIO $ timeSeed (min minTime 0.1) seedData runOp startValue
        _   <- note "ran %d iterations in %s\n" testIters (secs testTime)
        cfg <- getConfig

        let testItersD       = fromIntegral testIters
        let sampleCount      = fromLJ cfgSamples cfg
        let timePer          = (testTime - testItersD * clockCost) / testItersD
        let chunkSizeD       = minTime / timePer
        let chunkSize        = min (V.length workData) (ceiling chunkSizeD)
        let nSamples1        = min (chunkSize * sampleCount) (V.length workData)
        let numItersD        = fromIntegral nSamples1 / fromIntegral chunkSize
        let nSamples = max 1 (floor numItersD * chunkSize)

        _ <- note "collecting %d samples (in chunks of %d) in estimated %s\n"
                  nSamples chunkSize
                  (secs ((chunkSizeD * timePer + clockCost)*numItersD))


        (sample,_) <- mkSample chunkSize nSamples workData startValue' runOp
        liftIO performGC
        return (SampleData inputSize chunkSize sample)


    --------------------------------------------------------------------------
    mkSample chunkSize nSamples workData startValue runOp = liftIO $ do
        valRef <- newIORef startValue

        let numItersD = fromIntegral nSamples / fromIntegral chunkSize
        -- make sure nSamples is an integral multiple of chunkSize
        let numIters = max 1 (floor (numItersD :: Double))

        sample <- U.generateM numIters $ \chunk -> do
            !val <- readIORef valRef
            (!tm, val') <- time (runWorkData workData chunkSize runOp
                                             (chunk*chunkSize) 0 val)
            writeIORef valRef val'
            return $ tm - clockCost

        val <- readIORef valRef
        return (sample :: U.Vector Double, val)

    --------------------------------------------------------------------------
    clockCost = envClockCost env

    --------------------------------------------------------------------------
    timeSeed howLong seedData runOp startValue =
        loop startValue seedData (0::Int) =<< getTime
      where
        loop sv seed iters initTime = do
            now <- getTime
            let n = V.length seed
            when (now - initTime > howLong * 10) $
                 fail (printf "took too long to run: seed %d, iters %d"
                              (V.length seed) iters)
            (elapsed, (_,sv')) <- time (mkSample 1 n seed sv runOp)

            if elapsed < howLong
              then loop sv' (seed `mappend` seed) (iters+1) initTime
              else return (elapsed, n, sv')


------------------------------------------------------------------------------
takeSamples :: (NFData op) =>
               MeasurementMode
            -> WorkloadMode
            -> Benchmark op
            -> Environment
            -> GenIO
            -> Criterion [(String, [SampleData])]
takeSamples !mMode !wMode !benchmark !env !rng = do
    let szs = inputSizes benchmark
    when (null szs) $ fail "No input sizes defined"

    ssamples <- mapM (takeSample mMode wMode benchmark env rng) szs
    let names = map fst $ dataStructures benchmark
    let inputs = foldl' combine (map (const []) names) (reverse ssamples)

    return $ names `zip` inputs

  where
    combine :: [[SampleData]] -> [SampleData] -> [[SampleData]]
    combine int samples = map (uncurry (flip (:))) (int `zip` samples)
