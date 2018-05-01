{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Data.HashTable.Internal.Linear.Bucket
( Bucket,
  newBucketArray,
  newBucketSize,
  emptyWithSize,
  growBucketTo,
  snoc,
  size,
  lookup,
  lookupIndex,
  elemAt,
  delete,
  mutate,
  mutateST,
  toList,
  fromList,
  mapM_,
  foldM,
  expandBucketArray,
  expandArray,
  nelemsAndOverheadInWords,
  bucketSplitSize
) where


------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad                        hiding (foldM, mapM_)
import qualified Control.Monad
import           Control.Monad.ST                     (ST)
#ifdef DEBUG
import           Data.HashTable.Internal.Utils        (unsafeIOToST)
#endif
import           Data.HashTable.Internal.Array
import           Data.Maybe                           (fromMaybe)
import           Data.STRef
import           Prelude                              hiding (lookup, mapM_)
------------------------------------------------------------------------------
import           Data.HashTable.Internal.UnsafeTricks


#ifdef DEBUG
import           System.IO
#endif


type Bucket s k v = Key (Bucket_ s k v)

------------------------------------------------------------------------------
data Bucket_ s k v = Bucket { _bucketSize :: {-# UNPACK #-} !Int
                            , _highwater  :: {-# UNPACK #-} !(STRef s Int)
                            , _keys       :: {-# UNPACK #-} !(MutableArray s k)
                            , _values     :: {-# UNPACK #-} !(MutableArray s v)
                            }


------------------------------------------------------------------------------
bucketSplitSize :: Int
bucketSplitSize = 16


------------------------------------------------------------------------------
newBucketArray :: Int -> ST s (MutableArray s (Bucket s k v))
newBucketArray k = newArray k emptyRecord

------------------------------------------------------------------------------
nelemsAndOverheadInWords :: Bucket s k v -> ST s (Int,Int)
nelemsAndOverheadInWords bKey = do
    if (not $ keyIsEmpty bKey)
      then do
        !hw <- readSTRef hwRef
        let !w = sz - hw
        return (hw, constOverhead + 2*w)
      else
        return (0, 0)

  where
    constOverhead = 8
    b             = fromKey bKey
    sz            = _bucketSize b
    hwRef         = _highwater b


------------------------------------------------------------------------------
emptyWithSize :: Int -> ST s (Bucket s k v)
emptyWithSize !sz = do
    !keys   <- newArray sz undefined
    !values <- newArray sz undefined
    !ref    <- newSTRef 0

    return $ toKey $ Bucket sz ref keys values


------------------------------------------------------------------------------
newBucketSize :: Int
newBucketSize = 4


------------------------------------------------------------------------------
expandArray  :: a                  -- ^ default value
             -> Int                -- ^ new size
             -> Int                -- ^ number of elements to copy
             -> MutableArray s a   -- ^ old array
             -> ST s (MutableArray s a)
expandArray def !sz !hw !arr = do
    newArr <- newArray sz def
    cp newArr

  where
    cp !newArr = go 0
      where
        go !i
          | i >= hw = return newArr
          | otherwise = do
                readArray arr i >>= writeArray newArr i
                go (i+1)


------------------------------------------------------------------------------
expandBucketArray :: Int
                  -> Int
                  -> MutableArray s (Bucket s k v)
                  -> ST s (MutableArray s (Bucket s k v))
expandBucketArray = expandArray emptyRecord


------------------------------------------------------------------------------
growBucketTo :: Int -> Bucket s k v -> ST s (Bucket s k v)
growBucketTo !sz bk | keyIsEmpty bk = emptyWithSize sz
                    | otherwise = do
    if osz >= sz
      then return bk
      else do
        hw <- readSTRef hwRef
        k' <- expandArray undefined sz hw keys
        v' <- expandArray undefined sz hw values
        return $ toKey $ Bucket sz hwRef k' v'

  where
    bucket = fromKey bk
    osz    = _bucketSize bucket
    hwRef  = _highwater bucket
    keys   = _keys bucket
    values = _values bucket


------------------------------------------------------------------------------
{-# INLINE snoc #-}
-- Just return == new bucket object
snoc :: Bucket s k v -> k -> v -> ST s (Int, Maybe (Bucket s k v))
snoc bucket | keyIsEmpty bucket = mkNew
            | otherwise         = snoc' (fromKey bucket)
  where
    mkNew !k !v = do
        debug "Bucket.snoc: mkNew"
        keys   <- newArray newBucketSize undefined
        values <- newArray newBucketSize undefined

        writeArray keys 0 k
        writeArray values 0 v
        ref <- newSTRef 1
        return (1, Just $ toKey $ Bucket newBucketSize ref keys values)

    snoc' (Bucket bsz hwRef keys values) !k !v =
        readSTRef hwRef >>= check
      where
        check !hw
          | hw < bsz  = bump hw
          | otherwise = spill hw

        bump hw = do
          debug $ "Bucket.snoc: bumping hw, bsz=" ++ show bsz ++ ", hw="
                    ++ show hw

          writeArray keys hw k
          writeArray values hw v
          let !hw' = hw + 1
          writeSTRef hwRef hw'
          debug "Bucket.snoc: finished"
          return (hw', Nothing)

        doublingThreshold = bucketSplitSize `div` 2
        growFactor = 1.5 :: Double
        newSize z | z == 0 = newBucketSize
                  | z < doublingThreshold = z * 2
                  | otherwise = ceiling $ growFactor * fromIntegral z

        spill !hw = do
            let sz = newSize bsz
            debug $ "Bucket.snoc: spilling, old size=" ++ show bsz ++ ", new size="
                      ++ show sz

            bk <- growBucketTo sz bucket

            debug "Bucket.snoc: spill finished, snoccing element"
            let (Bucket _ hwRef' keys' values') = fromKey bk

            let !hw' = hw+1
            writeArray keys' hw k
            writeArray values' hw v
            writeSTRef hwRef' hw'

            return (hw', Just bk)



------------------------------------------------------------------------------
{-# INLINE size #-}
size :: Bucket s k v -> ST s Int
size b | keyIsEmpty b = return 0
       | otherwise = readSTRef $ _highwater $ fromKey b


------------------------------------------------------------------------------
-- note: search in reverse order! We prefer recently snoc'd keys.
lookup :: (Eq k) => Bucket s k v -> k -> ST s (Maybe v)
lookup bucketKey !k | keyIsEmpty bucketKey = return Nothing
                    | otherwise = lookup' $ fromKey bucketKey
  where
    lookup' (Bucket _ hwRef keys values) = do
        hw <- readSTRef hwRef
        go (hw-1)
      where
        go !i
            | i < 0 = return Nothing
            | otherwise = do
                k' <- readArray keys i
                if k == k'
                  then do
                    !v <- readArray values i
                    return $! Just v
                  else go (i-1)

------------------------------------------------------------------------------
-- note: search in reverse order! We prefer recently snoc'd keys.
lookupIndex :: (Eq k) => Bucket s k v -> k -> ST s (Maybe Int)
lookupIndex bucketKey !k
  | keyIsEmpty bucketKey = return Nothing
  | otherwise = lookup' $ fromKey bucketKey
  where
    lookup' (Bucket _ hwRef keys _values) = do
        hw <- readSTRef hwRef
        go (hw-1)
      where
        go !i
            | i < 0 = return Nothing
            | otherwise = do
                k' <- readArray keys i
                if k == k'
                  then return (Just i)
                  else go (i-1)

elemAt :: Bucket s k v -> Int -> ST s (Maybe (k,v))
elemAt bucketKey ix
  | keyIsEmpty bucketKey = return Nothing
  | otherwise = lookup' $ fromKey bucketKey
  where
    lookup' (Bucket _ hwRef keys values) = do
        hw <- readSTRef hwRef
        if 0 <= ix && ix < hw
          then do k <- readArray keys ix
                  v <- readArray values ix
                  return (Just (k,v))
          else return Nothing

------------------------------------------------------------------------------
{-# INLINE toList #-}
toList :: Bucket s k v -> ST s [(k,v)]
toList bucketKey | keyIsEmpty bucketKey = return []
                 | otherwise = toList' $ fromKey bucketKey
  where
    toList' (Bucket _ hwRef keys values) = do
        hw <- readSTRef hwRef
        go [] hw 0
      where
        go !l !hw !i | i >= hw   = return l
                     | otherwise = do
            k <- readArray keys i
            v <- readArray values i
            go ((k,v):l) hw $ i+1


------------------------------------------------------------------------------
-- fromList needs to reverse the input in order to make fromList . toList == id
{-# INLINE fromList #-}
fromList :: [(k,v)] -> ST s (Bucket s k v)
fromList l = Control.Monad.foldM f emptyRecord (reverse l)
  where
    f bucket (k,v) = do
        (_,m) <- snoc bucket k v
        return $ fromMaybe bucket m

------------------------------------------------------------------------------
delete :: (Eq k) => Bucket s k v -> k -> ST s Bool
delete bucketKey !k | keyIsEmpty bucketKey = do
    debug $ "Bucket.delete: empty bucket"
    return False
                    | otherwise = do
    debug "Bucket.delete: start"
    del $ fromKey bucketKey
  where
    del (Bucket sz hwRef keys values) = do
        hw <- readSTRef hwRef
        debug $ "Bucket.delete: hw=" ++ show hw ++ ", sz=" ++ show sz
        go hw $ hw - 1

      where
        go !hw !i | i < 0 = return False
                  | otherwise = do
            k' <- readArray keys i
            if k == k'
              then do
                  debug $ "found entry to delete at " ++ show i
                  move (hw-1) i keys
                  move (hw-1) i values
                  let !hw' = hw-1
                  writeSTRef hwRef hw'
                  return True
              else go hw (i-1)


------------------------------------------------------------------------------
mutate :: (Eq k) =>
          Bucket s k v
       -> k
       -> (Maybe v -> (Maybe v, a))
       -> ST s (Int, Maybe (Bucket s k v), a)
mutate bucketKey !k !f = mutateST bucketKey k (pure . f)
{-# INLINE mutate #-}


------------------------------------------------------------------------------
mutateST :: (Eq k) =>
            Bucket s k v
         -> k
         -> (Maybe v -> ST s (Maybe v, a))
         -> ST s (Int, Maybe (Bucket s k v), a)
mutateST bucketKey !k !f
    | keyIsEmpty bucketKey = do
        fRes <- f Nothing
        case fRes of
            (Nothing, a) -> return (0, Nothing, a)
            (Just v', a) -> do
                (!hw', mbk) <- snoc bucketKey k v'
                return (hw', mbk, a)
    | otherwise = mutate' $ fromKey bucketKey
  where
    mutate' (Bucket sz hwRef keys values) = do
        hw <- readSTRef hwRef
        pos <- findPosition hw (hw-1)
        mv <- do
            if pos < 0
                then return Nothing
                else readArray values pos >>= return . Just
        fRes <- f mv
        case (mv, fRes) of
            (Nothing, (Nothing, a)) -> return (hw, Nothing, a)
            (Nothing, (Just v', a)) -> do
                (!hw', mbk) <- snoc bucketKey k v'
                return (hw', mbk, a)
            (Just v, (Just v', a)) -> do
                writeArray values pos v'
                return (hw, Nothing, a)
            (Just v, (Nothing, a)) -> do
                move (hw-1) pos keys
                move (hw-1) pos values
                let !hw' = hw-1
                writeSTRef hwRef hw'
                return (hw', Nothing, a)
      where
        findPosition !hw !i
            | i < 0 = return (-1)
            | otherwise = do
                k' <- readArray keys i
                if k == k'
                  then return i
                  else findPosition hw (i-1)


------------------------------------------------------------------------------
{-# INLINE mapM_ #-}
mapM_ :: ((k,v) -> ST s a) -> Bucket s k v -> ST s ()
mapM_ f bucketKey
    | keyIsEmpty bucketKey = do
        debug $ "Bucket.mapM_: bucket was empty"
        return ()
    | otherwise = doMap $ fromKey bucketKey
  where
    doMap (Bucket sz hwRef keys values) = do
        hw <- readSTRef hwRef
        debug $ "Bucket.mapM_: hw was " ++ show hw ++ ", sz was " ++ show sz
        go hw 0
      where
        go !hw !i | i >= hw = return ()
                  | otherwise = do
            k <- readArray keys i
            v <- readArray values i
            _ <- f (k,v)
            go hw $ i+1


------------------------------------------------------------------------------
{-# INLINE foldM #-}
foldM :: (a -> (k,v) -> ST s a) -> a -> Bucket s k v -> ST s a
foldM f !seed0 bucketKey
    | keyIsEmpty bucketKey = return seed0
    | otherwise = doMap $ fromKey bucketKey
  where
    doMap (Bucket _ hwRef keys values) = do
        hw <- readSTRef hwRef
        go hw seed0 0
      where
        go !hw !seed !i | i >= hw = return seed
                        | otherwise = do
            k <- readArray keys i
            v <- readArray values i
            seed' <- f seed (k,v)
            go hw seed' (i+1)


------------------------------------------------------------------------------
-- move i into j
move :: Int -> Int -> MutableArray s a -> ST s ()
move i j arr | i == j    = do
    debug $ "move " ++ show i ++ " into " ++ show j
    return ()
             | otherwise = do
    debug $ "move " ++ show i ++ " into " ++ show j
    readArray arr i >>= writeArray arr j



{-# INLINE debug #-}
debug :: String -> ST s ()

#ifdef DEBUG
debug s = unsafeIOToST $ do
              putStrLn s
              hFlush stdout
#else
#ifdef TESTSUITE
debug !s = do
    let !_ = length s
    return $! ()
#else
debug _ = return ()
#endif
#endif

