{-# LANGUAGE CPP #-}

module Criterion.Collection.Main
  ( CriterionCollectionConfig
  , defaultCriterionCollectionConfig
  , runBenchmark
  ) where

import           Control.DeepSeq
import           Control.Monad.Trans
import           Criterion.Collection.Sample
import           Criterion.Config
import           Criterion.Environment
import           Criterion.Measurement (secs)
import           Criterion.Monad
import           Data.List
import           System.IO
import           System.Random.MWC (GenIO)
import qualified System.Random.MWC as R
import           Text.CSV

#ifdef CHART
import           Criterion.Collection.Chart
#endif


data CriterionCollectionConfig = Cfg {
      _criterionConfig :: Config
    , _logPlot         :: Bool
      -- todo: more here
}


defaultCriterionCollectionConfig :: CriterionCollectionConfig
defaultCriterionCollectionConfig = Cfg defaultConfig False


-- Fixme: fold chart output into config and generalize to other post-processing
-- functions (like alternative chart types and CSV output)
runBenchmark :: (NFData op)
             => MeasurementMode
             -> WorkloadMode
             -> Benchmark op
             -> CriterionCollectionConfig
             -> Maybe FilePath
             -> IO ()
runBenchmark mMode wMode benchmark (Cfg cfg logPlot) fp = withConfig cfg $ do
    rng      <- liftIO $ R.withSystemRandom (\r -> return r :: IO GenIO)
    env      <- measureEnvironment
    plotData <- takeSamples mMode wMode benchmark env rng

    liftIO $ mkChart logPlot (benchmarkName benchmark) fp plotData
    liftIO $ mkCSV (benchmarkName benchmark) fp plotData


------------------------------------------------------------------------------
mkCSV :: String
      -> Maybe FilePath
      -> [(String, [SampleData])]
      -> IO ()
mkCSV chartTitle output plotData = do
    h <- maybe (return stdout)
               (\f -> openFile (f ++ ".csv") WriteMode)
               output

    hPutStr h $ printCSV allRows
    maybe (return ())
          (\_ -> hClose h)
          output

  where

    header = [ "Data Structure"
             , "Input Size"
             , "Mean (secs)"
             , "Stddev (secs)"
             , "95% (secs)"
             , "Max (secs)" ]

    allRows = header : sampleRows
    sampleRows = concatMap samplesToRows plotData

    samplesToRows (name, sds) = map (sampleToRow name) sds

    sampleToRow name sd =
        [ name
        , show inputSize
        , show mean
        , show stddev
        , show ninetyFifth
        , show maxVal ]
      where
        (mean, stddev) = computeMeanAndStddev sd
        ninetyFifth    = compute95thPercentile sd
        maxVal         = computeMax sd
        inputSize      = sdInputSize sd


------------------------------------------------------------------------------
mkChart :: Bool
        -> String
        -> Maybe FilePath
        -> [(String, [SampleData])]
        -> IO ()
#ifdef CHART
mkChart logPlot chartTitle output plotData' = do
    go output
    printChartResults chartTitle plotData'

  where
    plotData = map (\(a,(b,c)) -> (a,b,c)) (defaultColors `zip` plotData')

    go Nothing = do
        let chart = errBarChart logPlot 2.5 chartTitle plotData
        _ <- renderableToWindow chart 1024 768
        return ()

    go (Just fn) = do
        let chart = errBarChart logPlot 1.5 chartTitle plotData
        _ <- renderableToPNGFile chart 800 600 fn
        return ()


#else
-- FIXME
mkChart _ chartTitle _ plotData = printChartResults chartTitle plotData
#endif


------------------------------------------------------------------------------
printChartResults :: String
                  -> [(String, [SampleData])]
                  -> IO ()
printChartResults chartTitle plotData = do
    -- fixme
    putStrLn $ "Results for " ++ chartTitle
    dashes
    crlf
    mapM_ printOne plotData
  where
    dashes = putStrLn $ replicate 78 '-'
    crlf = putStrLn ""

    fieldSize = 14

    rpad s = if n > fieldSize
               then (take (fieldSize-2) s) ++ ".."
               else replicate nsp ' ' ++ s
      where
        n   = length s
        nsp = fieldSize-n

    lpad s = if n > fieldSize
               then (take (fieldSize-2) s) ++ ".."
               else s ++ replicate nsp ' '
      where
        n   = length s
        nsp = fieldSize-n

    printHeader = do
        putStrLn $ concat [ lpad "Input Sz", "  "
                          , lpad "Mean (secs)", "  "
                          , lpad "Stddev (secs)", "  "
                          , lpad "95% (secs)", "  "
                          , lpad "Max (secs)"]
        putStrLn $ concat [ replicate fieldSize '-', "  "
                          , replicate fieldSize '-', "  "
                          , replicate fieldSize '-', "  "
                          , replicate fieldSize '-', "  "
                          , replicate fieldSize '-' ]

    printOne (name, sampledata) = do
        putStrLn $ "Data structure " ++ name
        crlf
        printHeader
        mapM_ printSample sampledata
        crlf

    printSample sd = do
        --putStrLn $ "fixme: sample length is " ++ show sd
        let (mean,stddev) = computeMeanAndStddev sd
        let ninetyFifth   = compute95thPercentile sd
        let maxVal        = computeMax sd
        let inputSize = sdInputSize sd

        let f1 = rpad $ show inputSize
        let f2 = rpad $ secs mean
        let f3 = rpad $ secs stddev
        let f4 = rpad $ secs ninetyFifth
        let f5 = rpad $ secs maxVal

        putStrLn $ intercalate "  " [f1, f2, f3, f4, f5]

