{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

module Data.HashTable.Test.Common
  ( FixedTableType
  , dummyTable
  , forceType
  , tests
  ) where

------------------------------------------------------------------------------
import           Control.Monad                        (liftM, when)
import           Control.Monad.ST                     (unsafeIOToST)
import           Data.IORef
import           Data.List                            hiding ( insert
                                                             , delete
                                                             , lookup )
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V
import qualified Data.Vector.Mutable                  as MV
import           Prelude                              hiding (lookup, mapM_)
import           System.Random.MWC
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
------------------------------------------------------------------------------
import qualified Data.HashTable.Class                 as C
import           Data.HashTable.IO


------------------------------------------------------------------------------
type FixedTableType h = forall k v . IOHashTable h k v
type HashTest = forall h . C.HashTable h => String -> FixedTableType h -> Test
data SomeTest = SomeTest HashTest


------------------------------------------------------------------------------
assertEq :: (Eq a, Show a) =>
            String -> a -> a -> PropertyM IO ()
assertEq s expected got =
    when (expected /= got) $ do
      fail $ s ++ ": expected '" ++ show expected ++ "', got '"
               ++ show got ++ "'"


------------------------------------------------------------------------------
forceType :: forall m h k1 k2 v1 v2 . (Monad m, C.HashTable h) =>
             IOHashTable h k1 v1 -> IOHashTable h k2 v2 -> m ()
forceType _ _ = return ()


------------------------------------------------------------------------------
dummyTable :: forall k v h . C.HashTable h => IOHashTable h k v
dummyTable = undefined


------------------------------------------------------------------------------
tests :: C.HashTable h => String -> FixedTableType h -> Test
tests prefix dummyArg = testGroup prefix $ map f ts
  where
    f (SomeTest ht) = ht prefix dummyArg

    ts = [ SomeTest testFromListToList
         , SomeTest testInsert
         , SomeTest testInsert2
         , SomeTest testNewAndInsert
         , SomeTest testGrowTable
         , SomeTest testDelete
         ]


------------------------------------------------------------------------------
testFromListToList :: HashTest
testFromListToList prefix dummyArg =
    testProperty (prefix ++ "/fromListToList") $
                 monadicIO $ do
                     rng <- initializeRNG
                     forAllM arbitrary $ prop rng

  where
    prop :: GenIO -> [(Int, Int)] -> PropertyM IO ()
    prop rng origL = do
        let l = V.toList $ shuffle rng $ V.fromList $ dedupe origL
        ht <- run $ fromList l
        l' <- run $ toList ht
        assertEq "fromList . toList == id" (sort l) (sort l')
        forceType dummyArg ht


------------------------------------------------------------------------------
testInsert :: HashTest
testInsert prefix dummyArg =
    testProperty (prefix ++ "/insert") $
                 monadicIO $ do
                     rng <- initializeRNG
                     forAllM arbitrary $ prop rng

  where
    prop :: GenIO -> ([(Int, Int)], (Int,Int)) -> PropertyM IO ()
    prop rng (origL, (k,v)) = do
        let l = V.toList $ shuffle rng $ V.fromList $ remove k $ dedupe origL
        assert $ all (\t -> fst t /= k) l

        ht <- run $ fromList l
        nothing <- run $ lookup ht k
        assertEq ("lookup " ++ show k) Nothing nothing

        run $ insert ht k v
        r <- run $ lookup ht k
        assertEq ("lookup2 " ++ show k) (Just v) r

        forceType dummyArg ht


------------------------------------------------------------------------------
testInsert2 :: HashTest
testInsert2 prefix dummyArg =
    testProperty (prefix ++ "/insert2") $
                 monadicIO $ do
                     rng <- initializeRNG
                     forAllM arbitrary $ prop rng

  where
    prop :: GenIO -> ([(Int, Int)], (Int,Int,Int)) -> PropertyM IO ()
    prop rng (origL, (k,v,v2)) = do
        let l = V.toList $ shuffle rng $ V.fromList $ dedupe origL
        ht   <- run $ fromList l

        run $ insert ht k v
        r <- run $ lookup ht k
        assertEq ("lookup1 " ++ show k) (Just v) r

        run $ insert ht k v2
        r' <- run $ lookup ht k
        assertEq ("lookup2 " ++ show k) (Just v2) r'

        forceType dummyArg ht


------------------------------------------------------------------------------
testNewAndInsert :: HashTest
testNewAndInsert prefix dummyArg =
    testProperty (prefix ++ "/newAndInsert") $
                 monadicIO $ forAllM arbitrary prop

  where
    prop :: (Int,Int,Int) -> PropertyM IO ()
    prop (k,v,v2) = do
        ht <- run new

        nothing <- run $ lookup ht k
        assertEq ("lookup " ++ show k) Nothing nothing

        run $ insert ht k v
        r <- run $ lookup ht k
        assertEq ("lookup2 " ++ show k) (Just v) r

        run $ insert ht k v2
        r' <- run $ lookup ht k
        assertEq ("lookup3 " ++ show k) (Just v2) r'

        ctRef <- run $ newIORef (0::Int)
        run $ mapM_ (const $ modifyIORef ctRef (+1)) ht

        ct <- run $ readIORef ctRef
        assertEq "count = 1" 1 ct

        ct' <- run $ foldM (\i _ -> return $! i+1) (0::Int) ht
        assertEq "count2 = 1" 1 ct'

        forceType dummyArg ht


------------------------------------------------------------------------------
testGrowTable :: HashTest
testGrowTable prefix dummyArg =
    testProperty (prefix ++ "/growTable") $
                 monadicIO $ forAllM generator prop

  where
    generator = choose (32,2048)

    go n = new >>= go' (0::Int)
      where
        go' !i !ht | i >= n = return ht
                   | otherwise = do
            insert ht i i
            go' (i+1) ht


    f (!m,!s) (!k,!v) = return $! (max m k, v `seq` s+1)

    prop :: Int -> PropertyM IO ()
    prop n = do
        ht <- run $ go n
        i <- liftM head $ run $ sample' $ choose (0,n-1)

        v <- run $ lookup ht i
        assertEq ("lookup " ++ show i) (Just i) v

        ct <- run $ foldM f (0::Int, 0::Int) ht
        assertEq "max + count" (n-1,n) ct
        forceType dummyArg ht


------------------------------------------------------------------------------
testDelete :: HashTest
testDelete prefix dummyArg =
    testProperty (prefix ++ "/delete") $
                 monadicIO $ forAllM generator prop

  where
    generator = choose (32,2048)

    go n = new >>= go' (0::Int)
      where
        go' !i !ht | i >= n = return ht
                   | otherwise = do
            insert ht i i

            case i of
              3  -> do
                       delete ht 2
                       delete ht 3
                       insert ht 2 2
                       
              _  -> if i `mod` 2 == 0
                      then do
                        delete ht i
                        insert ht i i
                      else return ()

            go' (i+1) ht


    f (!m,!s) (!k,!v) = return $! (max m k, v `seq` s+1)

    prop :: Int -> PropertyM IO ()
    prop n = do
        ht <- run $ go n

        i <- liftM head $ run $ sample' $ choose (4,n-1)
        v <- run $ lookup ht i
        assertEq ("lookup " ++ show i) (Just i) v

        v3 <- run $ lookup ht 3
        assertEq ("lookup 3") Nothing v3

        ct <- run $ foldM f (0::Int, 0::Int) ht
        assertEq "max + count" (n-1,n-1) ct
        forceType dummyArg ht


------------------------------------------------------------------------------
initializeRNG :: PropertyM IO GenIO
initializeRNG = run $ withSystemRandom (return :: GenIO -> IO GenIO)


------------------------------------------------------------------------------
dedupe :: (Ord k, Ord v, Eq k) => [(k,v)] -> [(k,v)]
dedupe l = go0 $ sort l
  where
    go0 [] = []
    go0 (x:xs) = go id x xs

    go !dl !lastOne [] = (dl . (lastOne:)) []

    go !dl !lastOne@(!lx,_) ((x,v):xs) =
        if lx == x
          then go dl lastOne xs
          else go (dl . (lastOne:)) (x,v) xs


------------------------------------------------------------------------------
-- assumption: list is sorted.
remove :: (Ord k, Eq k) => k -> [(k,v)] -> [(k,v)]
remove m l = go id l
  where
    go !dl [] = dl []
    go !dl ll@((k,v):xs) =
        case compare k m of
             LT -> go (dl . ((k,v):)) xs
             EQ -> go dl xs
             GT -> dl ll


------------------------------------------------------------------------------
shuffle :: GenIO -> Vector k -> Vector k
shuffle rng v = if V.null v then v else V.modify go v
  where
    !n = V.length v

    go mv = f (n-1)
      where
        -- note: inclusive
        pickOne b = unsafeIOToST $ uniformR (0,b) rng

        swap = MV.unsafeSwap mv

        f 0  = return ()
        f !k = do
            idx <- pickOne k
            swap k idx
            f (k-1)

