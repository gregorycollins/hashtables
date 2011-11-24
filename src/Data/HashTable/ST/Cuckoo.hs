{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

{-|

A hash table using the cuckoo strategy. (See
<http://en.wikipedia.org/wiki/Cuckoo_hashing>). Use this hash table if you...

  * want the fastest possible inserts, and very fast lookups.

  * are conscious of memory usage; this table has less space overhead than
    "Data.HashTable.ST.Basic", but more than "Data.HashTable.ST.Linear".

  * don't care that a table resize might pause for a long time to rehash all
    of the key-value mappings.


/Details:/

The basic idea of cuckoo hashing, first introduced by Pagh and Rodler in 2001,
is to use /d/ hash functions instead of only one; in this implementation d=2
and the strategy we use is to split up a flat array of slots into @k@ buckets,
each cache-line-sized:

@
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+----------+
|x0|x1|x2|x3|x4|x5|x6|x7|y0|y1|y2|y3|y4|y5|y6|y7|z0|z1|z2........|
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+----------+
[  ^^^  bucket 0  ^^^  ][  ^^^  bucket 1  ^^^  ]...
@

There are actually three parallel arrays: one unboxed array of 'Int's for hash
codes, one boxed array for keys, and one boxed array for values. When looking
up a key-value mapping, we hash the key using two hash functions and look in
both buckets in the hash code array for the key. Each bucket is cache-line
sized, with its keys in no particular order. Because the hash code array is
unboxed, we can search it for the key using a highly-efficient branchless
strategy in C code, using SSE instructions if available.

On insert, if both buckets are full, we knock out a randomly-selected entry
from one of the buckets (using a random walk ensures that \"key cycles\" are
broken with maximum probability) and try to repeat the insert procedure. This
process may not succeed; if all items have not successfully found a home after
some number of tries, we give up and rehash all of the elements into a larger
table.

/Space overhead: experimental results/

The implementation of cuckoo hash given here is almost as fast for lookups as
the basic open-addressing hash table using linear probing, and on average is
more space-efficient: in randomized testing on my 64-bit machine (see
@test\/compute-overhead\/ComputeOverhead.hs@ in the source distribution), mean
overhead is 1.71 machine words per key-value mapping, with a standard deviation
of 0.30 words, and 2.46 words per mapping at the 95th percentile.


/References:/

  * A. Pagh and F. Rodler. Cuckoo hashing. In /Proceedings of the 9th
    Annual European Symposium on Algorithms/, pp. 121-133, 2001.
-}


module Data.HashTable.ST.Cuckoo
  ( HashTable
  , new
  , newSized
  , delete
  , lookup
  , insert
  , mapM_
  , foldM
  ) where


------------------------------------------------------------------------------
import           Control.Monad hiding (foldM, mapM_)
import           Control.Monad.ST
import           Data.Hashable hiding (hash)
import qualified Data.Hashable as H
import           Data.Int
import           Data.Maybe
import           Data.Primitive.Array
import           Data.STRef
import           GHC.Exts
import           Prelude hiding ( lookup, read, mapM_ )
------------------------------------------------------------------------------
import qualified Data.HashTable.Class                 as C
import           Data.HashTable.Internal.CheapPseudoRandomBitStream
import           Data.HashTable.Internal.CacheLine
import qualified Data.HashTable.Internal.IntArray     as U
import           Data.HashTable.Internal.Utils

#ifdef DEBUG
import           System.IO
#endif


------------------------------------------------------------------------------
-- | A cuckoo hash table.
newtype HashTable s k v = HT (STRef s (HashTable_ s k v))

data HashTable_ s k v = HashTable
    { _size    :: {-# UNPACK #-} !Int     -- ^ in buckets, total size is
                                          -- numWordsInCacheLine * _size
    , _rng     :: {-# UNPACK #-} !(BitStream s)
    , _hashes  :: {-# UNPACK #-} !(U.IntArray s)
    , _keys    :: {-# UNPACK #-} !(MutableArray s k)
    , _values  :: {-# UNPACK #-} !(MutableArray s v)
    , _maxAttempts :: {-# UNPACK #-} !Int
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
new = newSizedReal 2 >>= newRef
{-# INLINE new #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:newSized".
newSized :: Int -> ST s (HashTable s k v)
newSized n = do
    let n' = (n + numWordsInCacheLine - 1) `div` numWordsInCacheLine
    let k = nextBestPrime $ ceiling $ fromIntegral n' / maxLoad
    newSizedReal k >>= newRef
{-# INLINE newSized #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:insert".
insert :: (Eq k, Hashable k) => HashTable s k v -> k -> v -> ST s ()
insert ht !k !v = readRef ht >>= \h -> insert' h k v >>= writeRef ht


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:computeOverhead".
computeOverhead :: HashTable s k v -> ST s Double
computeOverhead htRef = readRef htRef >>= work
  where
    work (HashTable sz _ _ _ _ _) = do
        nFilled <- foldM f 0 htRef

        let oh = totSz                  -- one word per element in hashes
               + 2 * (totSz - nFilled)  -- two words per non-filled entry
               + 12                     -- fixed overhead

        return $! fromIntegral (oh::Int) / fromIntegral nFilled

      where
        totSz = numWordsInCacheLine * sz

        f !a _ = return $! a+1


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:delete".
delete :: (Hashable k, Eq k) =>
          HashTable s k v
       -> k
       -> ST s ()
delete htRef k = readRef htRef >>= go
  where
    go ht@(HashTable sz _ _ _ _ _) = do
        _ <- delete' ht False k b1 b2 h1 h2
        return ()

      where
        h1 = hash1 k
        h2 = hash2 k

        b1 = whichLine h1 sz
        b2 = whichLine h2 sz


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:lookup".
lookup :: (Eq k, Hashable k) =>
          HashTable s k v
       -> k
       -> ST s (Maybe v)
lookup htRef k = do
    ht <- readRef htRef
    lookup' ht k
{-# INLINE lookup #-}


------------------------------------------------------------------------------
lookup' :: (Eq k, Hashable k) =>
           HashTable_ s k v
        -> k
        -> ST s (Maybe v)
lookup' (HashTable sz _ hashes keys values _) !k = do
    -- Unlike the write case, prefetch doesn't seem to help here for lookup.
    -- prefetchRead hashes b2
    idx1 <- searchOne keys hashes k b1 h1

    if idx1 >= 0
      then do
        v <- readArray values idx1
        return $! Just v
      else do
        idx2 <- searchOne keys hashes k b2 h2
        if idx2 >= 0
          then do
            v <- readArray values idx2
            return $! Just v
          else
            return Nothing

  where
    h1 = hash1 k
    h2 = hash2 k

    b1 = whichLine h1 sz
    b2 = whichLine h2 sz
{-# INLINE lookup' #-}


------------------------------------------------------------------------------
searchOne :: (Eq k) =>
             MutableArray s k
          -> U.IntArray s
          -> k
          -> Int
          -> Int
          -> ST s Int
searchOne !keys !hashes !k = go
  where
    go !b !h = do
        debug $ "searchOne: go " ++ show b ++ " " ++ show h
        idx <- cacheLineSearch hashes b h
        debug $ "searchOne: cacheLineSearch returned " ++ show idx

        case idx of
          -1 -> return (-1)
          _  -> do
              k' <- readArray keys idx
              if k == k'
                then return idx
                else do
                  let !idx' = idx + 1
                  if isCacheLineAligned idx'
                    then return (-1)
                    else go idx' h
{-# INLINE searchOne #-}



------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:foldM".
foldM :: (a -> (k,v) -> ST s a)
      -> a
      -> HashTable s k v
      -> ST s a
foldM f seed0 htRef = readRef htRef >>= foldMWork f seed0
{-# INLINE foldM #-}


------------------------------------------------------------------------------
foldMWork :: (a -> (k,v) -> ST s a)
          -> a
          -> HashTable_ s k v
          -> ST s a
foldMWork f seed0 (HashTable sz _ hashes keys values _) = go 0 seed0
  where
    totSz = numWordsInCacheLine * sz

    go !i !seed | i >= totSz = return seed
                | otherwise  = do
        h <- U.readArray hashes i
        if h /= emptyMarker
          then do
            k <- readArray keys i
            v <- readArray values i
            !seed' <- f seed (k,v)
            go (i+1) seed'

          else
            go (i+1) seed
{-# INLINE foldMWork #-}


------------------------------------------------------------------------------
-- | See the documentation for this function in
-- "Data.HashTable.Class#v:mapM_".
mapM_ :: ((k,v) -> ST s a)
      -> HashTable s k v
      -> ST s ()
mapM_ f htRef = readRef htRef >>= mapMWork f
{-# INLINE mapM_ #-}


------------------------------------------------------------------------------
mapMWork :: ((k,v) -> ST s a)
         -> HashTable_ s k v
         -> ST s ()
mapMWork f (HashTable sz _ hashes keys values _) = go 0
  where
    totSz = numWordsInCacheLine * sz

    go !i | i >= totSz = return ()
          | otherwise  = do
        h <- U.readArray hashes i
        if h /= emptyMarker
          then do
            k <- readArray keys i
            v <- readArray values i
            _ <- f (k,v)
            go (i+1)
          else
            go (i+1)
{-# INLINE mapMWork #-}


---------------------------------
-- Private declarations follow --
---------------------------------


------------------------------------------------------------------------------
newSizedReal :: Int -> ST s (HashTable_ s k v)
newSizedReal nbuckets = do
    let !ntotal   = nbuckets * numWordsInCacheLine
    let !maxAttempts = 12 + (log2 $ toEnum nbuckets)

    debug $ "creating cuckoo hash table with " ++
            show nbuckets ++ " buckets having " ++
            show ntotal ++ " total slots"

    rng    <- newBitStream
    hashes <- U.newArray ntotal
    keys   <- newArray ntotal undefined
    values <- newArray ntotal undefined

    return $! HashTable nbuckets rng hashes keys values maxAttempts


insert' :: (Eq k, Hashable k) =>
           HashTable_ s k v
        -> k
        -> v
        -> ST s (HashTable_ s k v)
insert' ht k v = do
    debug "insert': begin"
    mbX <- updateOrFail ht k v
    z <- maybe (return ht)
               (\(k',v') -> grow ht k' v')
               mbX
    debug "insert': end"
    return z
{-# INLINE insert #-}


------------------------------------------------------------------------------
updateOrFail :: (Eq k, Hashable k) =>
                HashTable_ s k v
             -> k
             -> v
             -> ST s (Maybe (k,v))
updateOrFail ht@(HashTable sz _ hashes keys values _) k v = do
    debug $ "updateOrFail: begin: sz = " ++ show sz
    debug $ "   h1=" ++ show h1 ++ ", h2=" ++ show h2
            ++ ", b1=" ++ show b1 ++ ", b2=" ++ show b2
    (didx, hashCode) <- delete' ht True k b1 b2 h1 h2

    debug $ "delete' returned (" ++ show didx ++ "," ++ show hashCode ++ ")"

    if didx >= 0
      then do
        U.writeArray hashes didx hashCode
        writeArray keys didx k
        writeArray values didx v
        return Nothing
      else cuckoo

  where
    h1 = hash1 k
    h2 = hash2 k

    b1 = whichLine h1 sz
    b2 = whichLine h2 sz

    cuckoo = do
        debug "cuckoo: calling cuckooOrFail"
        result <- cuckooOrFail ht h1 h2 b1 b2 k v
        debug $ "cuckoo: cuckooOrFail returned " ++
                  (if isJust result then "Just _" else "Nothing")

        -- if cuckoo failed we need to grow the table.
        maybe (return Nothing)
              (return . Just)
              result
{-# INLINE updateOrFail #-}


------------------------------------------------------------------------------
-- Returns either (-1,-1) (not found, and both buckets full ==> trigger
-- cuckoo), or the slot in the array where it would be safe to write the given
-- key, and the hashcode to use there
delete' :: (Hashable k, Eq k) =>
           HashTable_ s k v     -- ^ hash table
        -> Bool                 -- ^ are we updating?
        -> k                    -- ^ key
        -> Int                  -- ^ cache line start address 1
        -> Int                  -- ^ cache line start address 2
        -> Int                  -- ^ hash1
        -> Int                  -- ^ hash2
        -> ST s (Int, Int)
delete' (HashTable _ _ hashes keys values _) !updating !k b1 b2 h1 h2 = do
    debug $ "delete' b1=" ++ show b1
              ++ " b2=" ++ show b2
              ++ " h1=" ++ show h1
              ++ " h2=" ++ show h2
    prefetchWrite hashes b2
    idx1 <- searchOne keys hashes k b1 h1
    if idx1 < 0
      then do
        idx2 <- searchOne keys hashes k b2 h2
        if idx2 < 0
          then if updating
                 then do
                   debug $ "delete': looking for empty element"
                   -- if we're updating, we look for an empty element
                   idxE1 <- cacheLineSearch hashes b1 emptyMarker
                   debug $ "delete': idxE1 was " ++ show idxE1
                   if idxE1 >= 0
                     then return (idxE1, h1)
                     else do
                       idxE2 <- cacheLineSearch hashes b2 emptyMarker
                       debug $ "delete': idxE2 was " ++ show idxE1
                       if idxE2 >= 0
                         then return (idxE2, h2)
                         else return (-1, -1)
                 else return (-1,-1)
          else deleteIt idx2 h2
      else deleteIt idx1 h1

  where
    deleteIt !idx !h = do
        if not updating
          then do
            U.writeArray hashes idx emptyMarker
            writeArray keys idx undefined
            writeArray values idx undefined
          else return ()
        return $! (idx, h)
{-# INLINE delete' #-}


------------------------------------------------------------------------------
cuckooOrFail :: (Hashable k, Eq k) =>
                HashTable_ s k v  -- ^ hash table
             -> Int               -- ^ hash code 1
             -> Int               -- ^ hash code 2
             -> Int               -- ^ cache line 1
             -> Int               -- ^ cache line 2
             -> k                 -- ^ key
             -> v                 -- ^ value
             -> ST s (Maybe (k,v))
cuckooOrFail (HashTable sz rng hashes keys values maxAttempts0)
                 !h1_0 !h2_0 !b1_0 !b2_0 !k0 !v0 = do
    -- at this point we know:
    --
    --   * there is no empty slot in either cache line
    --
    --   * the key doesn't already exist in the table
    --
    -- next things to do:
    --
    --   * decide which element to bump
    --
    --   * read that element, and write (k,v) in there
    --
    --   * attempt to write the bumped element into its other cache slot
    --
    --   * if it fails, recurse.

    debug $ "cuckooOrFail h1_0=" ++ show h1_0
              ++ " h2_0=" ++ show h2_0
              ++ " b1_0=" ++ show b1_0
              ++ " b2_0=" ++ show b2_0

    !lineChoice <- getNextBit rng

    debug $ "chose line " ++ show lineChoice
    let (!b, !h) = if lineChoice == 0 then (b1_0, h1_0) else (b2_0, h2_0)
    go b h k0 v0 maxAttempts0


  where
    randomIdx !b = do
        !z <- getNBits cacheLineIntBits rng
        return $! b + z

    bumpIdx !idx !h !k !v = do
        debug $ "bumpIdx idx=" ++ show idx ++ " h=" ++ show h
        !h' <- U.readArray hashes idx
        debug $ "bumpIdx: h' was " ++ show h'
        !k' <- readArray keys idx
        v'  <- readArray values idx
        U.writeArray hashes idx h
        writeArray keys idx k
        writeArray values idx v
        debug $ "bumped key with h'=" ++ show h'
        return $! (h', k', v')

    otherHash h k = if h2 == h then h1 else h2
      where
        h1 = hash1 k
        h2 = hash2 k

    tryWrite !b !h k v maxAttempts = do
        debug $ "tryWrite b=" ++ show b ++ " h=" ++ show h
        idx <- cacheLineSearch hashes b emptyMarker
        debug $ "cacheLineSearch returned " ++ show idx

        if idx >= 0
          then do
            U.writeArray hashes idx h
            writeArray keys idx k
            writeArray values idx v
            return Nothing
          else go b h k v $! maxAttempts - 1

    go !b !h !k v !maxAttempts | maxAttempts == 0 = return $! Just (k,v)
                               | otherwise = do
        idx <- randomIdx b
        (!h0', !k', v') <- bumpIdx idx h k v
        let !h' = otherHash h0' k'
        let !b' = whichLine h' sz

        tryWrite b' h' k' v' maxAttempts


------------------------------------------------------------------------------
grow :: (Eq k, Hashable k) =>
        HashTable_ s k v
     -> k
     -> v
     -> ST s (HashTable_ s k v)
grow (HashTable sz _ hashes keys values _) k0 v0 = do
    newHt <- grow' $! bumpSize sz

    mbR <- updateOrFail newHt k0 v0
    maybe (return newHt)
          (\_ -> grow' $ bumpSize $ _size newHt)
          mbR

  where
    grow' newSz = do
        debug $ "growing table, oldsz = " ++ show sz ++
                ", newsz=" ++ show newSz
        newHt <- newSizedReal newSz
        rehash newSz newHt


    rehash !newSz !newHt = go 0
      where
        totSz = numWordsInCacheLine * sz

        go !i | i >= totSz = return newHt
              | otherwise  = do
            h <- U.readArray hashes i
            if (h /= emptyMarker)
              then do
                k <- readArray keys i
                v <- readArray values i

                mbR <- updateOrFail newHt k v
                maybe (go $ i + 1)
                      (\_ -> grow' $ bumpSize newSz)
                      mbR
              else go $ i + 1


------------------------------------------------------------------------------
hashPrime :: Int
hashPrime = if wordSize == 32 then hashPrime32 else hashPrime64
  where
    hashPrime32 = 0xedf2a025
    hashPrime64 = 0x3971ca9c8b3722e9


------------------------------------------------------------------------------
hash1 :: Hashable k => k -> Int
hash1 = hashF H.hash
{-# INLINE hash1 #-}


hash2 :: Hashable k => k -> Int
hash2 = hashF (H.hashWithSalt hashPrime)
{-# INLINE hash2 #-}


hashF :: (k -> Int) -> k -> Int
hashF f k = out
  where
    !(I# h#) = f k

    !m#  = maskw# h# 0#
    !nm# = not# m#

    !r#  = ((int2Word# 1#) `and#` m#) `or#` (int2Word# h# `and#` nm#)
    !out = I# (word2Int# r#)
{-# INLINE hashF #-}


------------------------------------------------------------------------------
emptyMarker :: Int
emptyMarker = 0


------------------------------------------------------------------------------
maxLoad :: Double
maxLoad = 0.88


------------------------------------------------------------------------------
debug :: String -> ST s ()
#ifdef DEBUG
debug s = unsafeIOToST (putStrLn s >> hFlush stdout)
#else
debug _ = return ()
#endif
{-# INLINE debug #-}


------------------------------------------------------------------------------
whichLine :: Int -> Int -> Int
whichLine !h !sz = whichBucket h sz `iShiftL` cacheLineIntBits
{-# INLINE whichLine #-}


------------------------------------------------------------------------------
newRef :: HashTable_ s k v -> ST s (HashTable s k v)
newRef = liftM HT . newSTRef
{-# INLINE newRef #-}

writeRef :: HashTable s k v -> HashTable_ s k v -> ST s ()
writeRef (HT ref) ht = writeSTRef ref ht
{-# INLINE writeRef #-}

readRef :: HashTable s k v -> ST s (HashTable_ s k v)
readRef (HT ref) = readSTRef ref
{-# INLINE readRef #-}

