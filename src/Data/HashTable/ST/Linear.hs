{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE RankNTypes   #-}

{-| An implementation of linear hash tables. (See
<http://en.wikipedia.org/wiki/Linear_hashing>). Use this hash table if you...

  * don't care that inserts and lookups are slower than the other hash table
    implementations in this collection (this one is slightly faster than
    @Data.HashTable@ from the base library in most cases)

  * have a soft real-time or interactive application for which the risk of
    introducing a long pause on insert while all of the keys are rehashed is
    unacceptable.


/Details:/

Linear hashing allows for the expansion of the hash table one slot at a time,
by moving a \"split\" pointer across an array of pointers to buckets. The
number of buckets is always a power of two, and the bucket to look in is
defined as:

@
bucket(level,key) = hash(key) mod (2^level)
@

The \"split pointer\" controls the expansion of the hash table. If the hash
table is at level @k@ (i.e. @2^k@ buckets have been allocated), we first
calculate @b=bucket(level-1,key)@. If @b < splitptr@, the destination bucket is
calculated as @b'=bucket(level,key)@, otherwise the original value @b@ is used.

The split pointer is incremented once an insert causes some bucket to become
fuller than some predetermined threshold; the bucket at the split pointer
(*not* the bucket which triggered the split!) is then rehashed, and half of its
keys can be expected to be rehashed into the upper half of the table.

When the split pointer reaches the middle of the bucket array, the size of the
bucket array is doubled, the level increases, and the split pointer is reset to
zero.

Linear hashing, although not quite as fast for inserts or lookups as the
implementation of linear probing included in this package, is well suited for
interactive applications because it has much better worst case behaviour on
inserts. Other hash table implementations can suffer from long pauses, because
it is occasionally necessary to rehash all of the keys when the table grows.
Linear hashing, on the other hand, only ever rehashes a bounded (effectively
constant) number of keys when an insert forces a bucket split.

/Space overhead: experimental results/

In randomized testing (see @test\/compute-overhead\/ComputeOverhead.hs@ in the
source distribution), mean overhead is approximately 1.51 machine words per
key-value mapping with a very low standard deviation of about 0.06 words, 1.60
words per mapping at the 95th percentile.

/Unsafe tricks/

Then the @unsafe-tricks@ flag is on when this package is built (and it is on by
default), we use some unsafe tricks (namely 'unsafeCoerce#' and
'reallyUnsafePtrEquality#') to save indirections in this table. These
techniques rely on assumptions about the behaviour of the GHC runtime system
and, although they've been tested and should be safe under normal conditions,
are slightly dangerous. Caveat emptor. In particular, these techniques are
incompatible with HPC code coverage reports.


References:

  * W. Litwin. Linear hashing: a new tool for file and table addressing. In
    /Proc. 6th International Conference on Very Large Data Bases, Volume 6/,
    pp. 212-223, 1980.

  * P-A. Larson. Dynamic hash tables. /Communications of the ACM/ 31:
    446-457, 1988.
-}

module Data.HashTable.ST.Linear
  ( HashTable
  , new
  , newSized
  , delete
  , lookup
  , insert
  , mapM_
  , foldM
  , computeOverhead
  ) where

------------------------------------------------------------------------------
import           Control.Monad                         hiding (foldM, mapM_)
import           Control.Monad.ST
import           Data.Bits
import           Data.Hashable
import           Data.STRef
import           Prelude                               hiding (lookup, mapM_)
------------------------------------------------------------------------------
import qualified Data.HashTable.Class                  as C
import           Data.HashTable.Internal.Array
import           Data.HashTable.Internal.Linear.Bucket (Bucket)
import qualified Data.HashTable.Internal.Linear.Bucket as Bucket
import           Data.HashTable.Internal.Utils

#ifdef DEBUG
import           System.IO
#endif


------------------------------------------------------------------------------
-- | A linear hash table.
newtype HashTable s k v = HT (STRef s (HashTable_ s k v))

data HashTable_ s k v = HashTable
    { _level    :: {-# UNPACK #-} !Int
    , _splitptr :: {-# UNPACK #-} !Int
    , _buckets  :: {-# UNPACK #-} !(MutableArray s (Bucket s k v))
    }


------------------------------------------------------------------------------
instance C.HashTable HashTable where
    new             = new
    newSized        = newSized
    insert          = insert
    delete          = delete
    lookup          = lookup
    foldM           = foldM
    mapM_           = mapM_
    computeOverhead = computeOverhead


------------------------------------------------------------------------------
instance Show (HashTable s k v) where
    show _ = "<HashTable>"


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:new".
new :: ST s (HashTable s k v)
new = do
    v <- Bucket.newBucketArray 2
    newRef $ HashTable 1 0 v


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:newSized".
newSized :: Int -> ST s (HashTable s k v)
newSized n = do
    v <- Bucket.newBucketArray sz
    newRef $ HashTable lvl 0 v

  where
    k   = ceiling (fromIntegral n * fillFactor / fromIntegral bucketSplitSize)
    lvl = max 1 (fromEnum $ log2 k)
    sz  = power2 lvl



------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:delete".
delete :: (Hashable k, Eq k) =>
          (HashTable s k v)
       -> k
       -> ST s ()
delete htRef !k = readRef htRef >>= work
  where
    work (HashTable lvl splitptr buckets) = do
        let !h0 = hashKey lvl splitptr k
        debug $ "delete: size=" ++ show (power2 lvl) ++ ", h0=" ++ show h0
                  ++ "splitptr: " ++ show splitptr
        delete' buckets h0 k
{-# INLINE delete #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:lookup".
lookup :: (Eq k, Hashable k) => (HashTable s k v) -> k -> ST s (Maybe v)
lookup htRef !k = readRef htRef >>= work
  where
    work (HashTable lvl splitptr buckets) = do
        let h0 = hashKey lvl splitptr k
        bucket <- readArray buckets h0
        Bucket.lookup bucket k
{-# INLINE lookup #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:insert".
insert :: (Eq k, Hashable k) =>
          (HashTable s k v)
       -> k
       -> v
       -> ST s ()
insert htRef k v = do
    ht' <- readRef htRef >>= work
    writeRef htRef ht'
  where
    work ht@(HashTable lvl splitptr buckets) = do
        let !h0 = hashKey lvl splitptr k
        delete' buckets h0 k
        bsz <- primitiveInsert' buckets h0 k v

        if checkOverflow bsz
          then do
            debug $ "insert: splitting"
            h <- split ht
            debug $ "insert: done splitting"
            return h
          else do
            debug $ "insert: done"
            return ht
{-# INLINE insert #-}



------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:mapM_".
mapM_ :: ((k,v) -> ST s b) -> HashTable s k v -> ST s ()
mapM_ f htRef = readRef htRef >>= work
  where
    work (HashTable lvl _ buckets) = go 0
      where
        !sz = power2 lvl

        go !i | i >= sz = return ()
              | otherwise = do
            b <- readArray buckets i
            Bucket.mapM_ f b
            go $ i+1


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:foldM".
foldM :: (a -> (k,v) -> ST s a)
      -> a -> HashTable s k v
      -> ST s a
foldM f seed0 htRef = readRef htRef >>= work
  where
    work (HashTable lvl _ buckets) = go seed0 0
      where
        !sz = power2 lvl

        go !seed !i | i >= sz   = return seed
                    | otherwise = do
            b <- readArray buckets i
            !seed' <- Bucket.foldM f seed b
            go seed' $ i+1


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:computeOverhead".
computeOverhead :: HashTable s k v -> ST s Double
computeOverhead htRef = readRef htRef >>= work
  where
    work (HashTable lvl _ buckets) = do
        (totElems, overhead) <- go 0 0 0

        let n = fromIntegral totElems
        let o = fromIntegral overhead

        return $ (fromIntegral sz + constOverhead + o) / n

      where
        constOverhead = 5.0

        !sz = power2 lvl

        go !nelems !overhead !i | i >= sz = return (nelems, overhead)
                                | otherwise = do
            b <- readArray buckets i
            (!n,!o) <- Bucket.nelemsAndOverheadInWords b
            let !n' = n + nelems
            let !o' = o + overhead

            go n' o' (i+1)


------------------------------
-- Private functions follow --
------------------------------

------------------------------------------------------------------------------
delete' :: Eq k =>
           MutableArray s (Bucket s k v)
        -> Int
        -> k
        -> ST s ()
delete' buckets h0 k = do
    bucket <- readArray buckets h0
    _ <- Bucket.delete bucket k
    return ()


------------------------------------------------------------------------------
split :: (Hashable k) =>
         (HashTable_ s k v)
      -> ST s (HashTable_ s k v)
split ht@(HashTable lvl splitptr buckets) = do
    debug $ "split: start: nbuck=" ++ show (power2 lvl)
              ++ ", splitptr=" ++ show splitptr

    -- grab bucket at splitPtr
    oldBucket <- readArray buckets splitptr

    nelems <- Bucket.size oldBucket
    let !bsz = max Bucket.newBucketSize $
                   ceiling $ (0.625 :: Double) * fromIntegral nelems

    -- write an empty bucket there
    dbucket1 <- Bucket.emptyWithSize bsz
    writeArray buckets splitptr dbucket1

    -- grow the buckets?
    let lvl2 = power2 lvl
    let lvl1 = power2 $ lvl-1

    (!buckets',!lvl',!sp') <-
        if splitptr+1 >= lvl1
          then do
            debug $ "split: resizing bucket array"
            let lvl3 = 2*lvl2
            b <- Bucket.expandBucketArray lvl3 lvl2 buckets
            debug $ "split: resizing bucket array: done"
            return (b,lvl+1,0)
          else return (buckets,lvl,splitptr+1)

    let ht' = HashTable lvl' sp' buckets'

    -- make sure the other split bucket has enough room in it also
    let splitOffs = splitptr + lvl1
    db2   <- readArray buckets' splitOffs
    db2sz <- Bucket.size db2
    let db2sz' = db2sz + bsz
    db2'  <- Bucket.growBucketTo db2sz' db2
    debug $ "growing bucket at " ++ show splitOffs ++ " to size "
              ++ show db2sz'
    writeArray buckets' splitOffs db2'

    -- rehash old bucket
    debug $ "split: rehashing bucket"
    let f = uncurry $ primitiveInsert ht'
    forceSameType f (uncurry $ primitiveInsert ht)

    Bucket.mapM_ f oldBucket
    debug $ "split: done"
    return ht'


------------------------------------------------------------------------------
checkOverflow :: Int -> Bool
checkOverflow sz = sz > bucketSplitSize


------------------------------------------------------------------------------
-- insert w/o splitting
primitiveInsert :: (Hashable k) =>
                   (HashTable_ s k v)
                -> k
                -> v
                -> ST s Int
primitiveInsert (HashTable lvl splitptr buckets) k v = do
    debug $ "primitiveInsert start: nbuckets=" ++ show (power2 lvl)
    let h0 = hashKey lvl splitptr k
    primitiveInsert' buckets h0 k v


------------------------------------------------------------------------------
primitiveInsert' :: MutableArray s (Bucket s k v)
                 -> Int
                 -> k
                 -> v
                 -> ST s Int
primitiveInsert' buckets !h0 !k !v = do
    debug $ "primitiveInsert': bucket number=" ++ show h0
    bucket <- readArray buckets h0
    debug $ "primitiveInsert': snoccing bucket"
    (!hw,m) <- Bucket.snoc bucket k v
    debug $ "primitiveInsert': bucket snoc'd"
    maybe (return ())
          (writeArray buckets h0)
          m
    return hw




------------------------------------------------------------------------------
fillFactor :: Double
fillFactor = 1.3


------------------------------------------------------------------------------
bucketSplitSize :: Int
bucketSplitSize = Bucket.bucketSplitSize


------------------------------------------------------------------------------
{-# INLINE power2 #-}
power2 :: Int -> Int
power2 i = 1 `iShiftL` i


------------------------------------------------------------------------------
{-# INLINE hashKey #-}
hashKey :: (Hashable k) => Int -> Int -> k -> Int
hashKey !lvl !splitptr !k = h1
  where
    !h0 = hashAtLvl (lvl-1) k
    !h1 = if (h0 < splitptr)
            then hashAtLvl lvl k
            else h0


------------------------------------------------------------------------------
{-# INLINE hashAtLvl #-}
hashAtLvl :: (Hashable k) => Int -> k -> Int
hashAtLvl !lvl !k = h
  where
    !h        = hashcode .&. mask
    !hashcode = hash k
    !mask     = power2 lvl - 1


------------------------------------------------------------------------------
newRef :: HashTable_ s k v -> ST s (HashTable s k v)
newRef = liftM HT . newSTRef

writeRef :: HashTable s k v -> HashTable_ s k v -> ST s ()
writeRef (HT ref) ht = writeSTRef ref ht

readRef :: HashTable s k v -> ST s (HashTable_ s k v)
readRef (HT ref) = readSTRef ref


------------------------------------------------------------------------------
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

