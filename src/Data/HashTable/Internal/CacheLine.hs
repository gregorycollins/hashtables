{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}

module Data.HashTable.Internal.CacheLine 
  ( cacheLineSearch
  , cacheLineSearch2
  , cacheLineSearch3
  , forwardSearch2
  , forwardSearch3
  , isCacheLineAligned
  , advanceByCacheLineSize
  , prefetchRead
  , prefetchWrite
  , bl_abs#
  , sign#
  , mask#
  , maskw#
  ) where

import           Control.Monad.ST
import           Data.HashTable.Internal.IntArray (IntArray)
import qualified Data.HashTable.Internal.IntArray as M

#ifndef NO_C_SEARCH
import           Foreign.C.Types
#else
import           Data.Bits
import           Data.Int
import qualified Data.Vector.Unboxed         as U
import           GHC.Int
#endif

import           Data.HashTable.Internal.Utils
import           GHC.Exts


{-# INLINE prefetchRead  #-}
{-# INLINE prefetchWrite #-}
prefetchRead :: IntArray s -> Int -> ST s ()
prefetchWrite :: IntArray s -> Int -> ST s ()

#ifndef NO_C_SEARCH
foreign import ccall unsafe "lineSearch32"
  c_lineSearch32 :: Ptr a -> CInt -> CUInt -> IO Int

foreign import ccall unsafe "lineSearch64"
  c_lineSearch64 :: Ptr a -> CInt -> CULong -> IO Int

foreign import ccall unsafe "lineSearch32_2"
  c_lineSearch32_2 :: Ptr a -> CInt -> CUInt -> CUInt -> IO Int

foreign import ccall unsafe "lineSearch64_2"
  c_lineSearch64_2 :: Ptr a -> CInt -> CULong -> CULong -> IO Int

foreign import ccall unsafe "lineSearch32_3"
  c_lineSearch32_3 :: Ptr a -> CInt -> CUInt -> CUInt -> CUInt -> IO Int

foreign import ccall unsafe "lineSearch64_3"
  c_lineSearch64_3 :: Ptr a -> CInt -> CULong -> CULong -> CULong -> IO Int

foreign import ccall unsafe "forwardSearch32_2"
  c_forwardSearch32_2 :: Ptr a -> CInt -> CInt -> CUInt -> CUInt -> IO Int

foreign import ccall unsafe "forwardSearch32_3"
  c_forwardSearch32_3 :: Ptr a -> CInt -> CInt -> CUInt -> CUInt -> CUInt
                      -> IO Int

foreign import ccall unsafe "forwardSearch64_2"
  c_forwardSearch64_2 :: Ptr a -> CInt -> CInt -> CULong -> CULong -> IO Int

foreign import ccall unsafe "forwardSearch64_3"
  c_forwardSearch64_3 :: Ptr a -> CInt -> CInt -> CULong -> CULong -> CULong
                      -> IO Int

foreign import ccall unsafe "prefetchCacheLine32_read"
  prefetchCacheLine32_read :: Ptr a -> CInt -> IO ()

foreign import ccall unsafe "prefetchCacheLine64_read"
  prefetchCacheLine64_read :: Ptr a -> CInt -> IO ()

foreign import ccall unsafe "prefetchCacheLine32_write"
  prefetchCacheLine32_write :: Ptr a -> CInt -> IO ()

foreign import ccall unsafe "prefetchCacheLine64_write"
  prefetchCacheLine64_write :: Ptr a -> CInt -> IO ()


fI :: (Num b, Integral a) => a -> b
fI = fromIntegral


prefetchRead a i = unsafeIOToST c
  where
    v   = M.toPtr a
    x   = fI i
    c32 = prefetchCacheLine32_read v x
    c64 = prefetchCacheLine64_read v x
    c   = if wordSize == 32 then c32 else c64


prefetchWrite a i = unsafeIOToST c
  where
    v   = M.toPtr a
    x   = fI i
    c32 = prefetchCacheLine32_write v x
    c64 = prefetchCacheLine64_write v x
    c   = if wordSize == 32 then c32 else c64


{-# INLINE forwardSearch2 #-}
forwardSearch2 :: IntArray s -> Int -> Int -> Int -> Int -> ST s Int
forwardSearch2 !vec !start !end !x1 !x2 = 
    unsafeIOToST c
  where
    c32 = c_forwardSearch32_2 (M.toPtr vec) (fI start) (fI end) (fI x1) (fI x2)
    c64 = c_forwardSearch64_2 (M.toPtr vec) (fI start) (fI end) (fI x1) (fI x2)
    c = if wordSize == 32 then c32 else c64


{-# INLINE forwardSearch3 #-}
forwardSearch3 :: IntArray s -> Int -> Int -> Int -> Int -> Int -> ST s Int
forwardSearch3 !vec !start !end !x1 !x2 !x3 = 
    unsafeIOToST c
  where
    c32 = c_forwardSearch32_3 (M.toPtr vec) (fI start) (fI end)
                              (fI x1) (fI x2) (fI x3)
    c64 = c_forwardSearch64_3 (M.toPtr vec) (fI start) (fI end)
                              (fI x1) (fI x2) (fI x3)
    c = if wordSize == 32 then c32 else c64


{-# INLINE lineSearch #-}
lineSearch :: IntArray s -> Int -> Int -> ST s Int
lineSearch !vec !start !value =
    unsafeIOToST c
  where
    c32 = c_lineSearch32 (M.toPtr vec) (fI start) (fI value)
    c64 = c_lineSearch64 (M.toPtr vec) (fI start) (fI value)
    c = if wordSize == 32 then c32 else c64

{-# INLINE lineSearch2 #-}
lineSearch2 :: IntArray s -> Int -> Int -> Int -> ST s Int
lineSearch2 !vec !start !x1 !x2 =
    unsafeIOToST c
  where
    c32 = c_lineSearch32_2 (M.toPtr vec) (fI start) (fI x1) (fI x2)
    c64 = c_lineSearch64_2 (M.toPtr vec) (fI start) (fI x1) (fI x2)
    c = if wordSize == 32 then c32 else c64

{-# INLINE lineSearch3 #-}
lineSearch3 :: IntArray s -> Int -> Int -> Int -> Int -> ST s Int
lineSearch3 !vec !start !x1 !x2 !x3 =
    unsafeIOToST c
  where
    c32 = c_lineSearch32_3 (M.toPtr vec) (fI start) (fI x1) (fI x2) (fI x3)
    c64 = c_lineSearch64_3 (M.toPtr vec) (fI start) (fI x1) (fI x2) (fI x3)
    c = if wordSize == 32 then c32 else c64
#endif

{-# INLINE advanceByCacheLineSize #-}
advanceByCacheLineSize :: Int -> Int -> Int
advanceByCacheLineSize !(I# start0#) !(I# vecSize#) = out
  where
    !(I# clm#) = cacheLineIntMask
    !clmm#     = not# (int2Word# clm#)
    !start#    = word2Int# (clmm# `and#` int2Word# start0#)
    !(I# nw#)  = numWordsInCacheLine
    !start'#   = start# +# nw#
    !s#        = sign# (vecSize# -# start'# -# 1#)
    !m#        = not# (int2Word# s#)
    !r#        = int2Word# start'# `and#` m#
    !out       = I# (word2Int# r#)


{-# INLINE isCacheLineAligned #-}
isCacheLineAligned :: Int -> Bool
isCacheLineAligned (I# x#) = r# ==# 0#
  where
    !(I# m#) = cacheLineIntMask
    !mw#     = int2Word# m#
    !w#      = int2Word# x#
    !r#      = word2Int# (mw# `and#` w#)


{-# INLINE sign# #-}
-- | Returns 0 if x is positive, -1 otherwise
sign# :: Int# -> Int#
sign# !x# = x# `uncheckedIShiftRA#` wordSizeMinus1#
  where
    !(I# wordSizeMinus1#) = wordSize-1


{-# INLINE bl_abs# #-}
-- | Abs of an integer, branchless
bl_abs# :: Int# -> Int#
bl_abs# !x# = word2Int# r#
  where
    !m# = sign# x#
    !r# = (int2Word# (m# +# x#)) `xor#` int2Word# m#

    
{-# INLINE mask# #-}
-- | Returns 0xfff..fff (aka -1) if a# == b#, 0 otherwise.
mask# :: Int# -> Int# -> Int#
mask# !a# !b# = dest#
  where
    !d#    = a# -# b#
    !r#    = bl_abs# d# -# 1#
    !dest# = sign# r#


{- note: this code should be:

mask# :: Int# -> Int# -> Int#
mask# !a# !b# = let !(I# z#) = fromEnum (a# ==# b#)
                    !q#      = negateInt# z#
                in q#

but GHC doesn't properly optimize this as straight-line code at the moment.

-}


{-# INLINE maskw# #-}
maskw# :: Int# -> Int# -> Word#
maskw# !a# !b# = int2Word# (mask# a# b#)


#ifdef NO_C_SEARCH
prefetchRead _ _ = return ()
prefetchWrite _ _ = return ()

{-# INLINE forwardSearch2 #-}
forwardSearch2 :: IntArray s -> Int -> Int -> Int -> Int -> ST s Int
forwardSearch2 !vec !start !end !x1 !x2 = go start end False
  where
    next !i !e !b = let !j = i+1
                    in if j == e
                         then (if b then (-1,e,True) else (0,start,True))
                         else (j,e,b)

    go !i !e !b = do
        h <- M.readArray vec i
        if h == x1 || h == x2
          then return i
          else do
              let (!i',!e',!b') = next i e b
              if (i' < 0) then return (-1) else go i' e' b'


{-# INLINE forwardSearch3 #-}
forwardSearch3 :: IntArray s -> Int -> Int -> Int -> Int -> Int -> ST s Int
forwardSearch3 !vec !start !end !x1 !x2 !x3 = go start end False
  where
    next !i !e !b = let !j = i+1
                    in if j == e
                         then (if b then (-1,e,True) else (0,start,True))
                         else (j,e,b)

    go !i !e !b = do
        h <- M.readArray vec i
        if h == x1 || h == x2 || h == x3
          then return i
          else do
              let (!i',!e',!b') = next i e b
              if (i' < 0) then return (-1) else go i' e' b'


deBruijnBitPositions :: U.Vector Int8
deBruijnBitPositions =
    U.fromList [
          0,   1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
          31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
         ]


{-# INLINE firstBitSet# #-}
-- only works with 32-bit values -- ok for us here
firstBitSet# :: Int# -> Int#
firstBitSet# i# = word2Int# ((or# zeroCase# posw#))
  where
    !zeroCase#   = int2Word# (mask# 0# i#)
    !w#          = int2Word# i#
    !iLowest#    = word2Int# (and# w# (int2Word# (negateInt# i#)))
    !idxW#       = uncheckedShiftRL#
                       (narrow32Word# (timesWord# (int2Word# iLowest#)
                                                  (int2Word# 0x077CB531#)))
                       27#
    !idx         = I# (word2Int# idxW#)
    !(I8# pos8#) = U.unsafeIndex deBruijnBitPositions idx
    !posw#       = int2Word# pos8#

#endif


------------------------------------------------------------------------------
-- | Search through a mutable vector for a given int value, cache-line aligned.
-- If the start index is cache-line aligned, and there is more than a
-- cache-line's room between the start index and the end of the vector, we will
-- search the cache-line all at once using an efficient branchless
-- bit-twiddling technique. Otherwise, we will use a typical loop.
--
cacheLineSearch :: IntArray s        -- ^ vector to search
                -> Int               -- ^ start index
                -> Int               -- ^ value to search for
                -> ST s Int          -- ^ dest index where it can be found, or
                                     -- \"-1\" if not found
cacheLineSearch !vec !start !value = do
#ifdef NO_C_SEARCH
    let !vlen  = M.length vec
    let !st1   = vlen - start
    let !nvlen = numWordsInCacheLine - st1
    let adv    = (start + cacheLineIntMask) .&. complement cacheLineIntMask
    let st2    = adv - start


    if nvlen > 0 || not (isCacheLineAligned start)
      then naiveSearch vec start (min st1 st2) value
      else lineSearch vec start value
#else
    lineSearch vec start value
#endif
{-# INLINE cacheLineSearch #-}


#ifdef NO_C_SEARCH
-- | Search through a mutable vector for a given int value. The number of
-- things to search for must be at most the number of things remaining in the
-- vector.
naiveSearch :: IntArray s       -- ^ vector to search
            -> Int              -- ^ start index
            -> Int              -- ^ number of things to search
            -> Int              -- ^ value to search for
            -> ST s Int         -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
naiveSearch !vec !start !nThings !value = go start
  where
    !doneIdx = start + nThings

    go !i | i >= doneIdx = return (-1)
          | otherwise = do
        x <- M.readArray vec i
        if x == value then return i else go (i+1)
{-# INLINE naiveSearch #-}


lineResult# :: Word#    -- ^ mask
            -> Int      -- ^ start value
            -> Int
lineResult# bitmask# (I# start#) = I# (word2Int# rv#)
  where
    !p#   = firstBitSet# (word2Int# bitmask#)
    !mm#  = maskw# p# (-1#)
    !nmm# = not# mm#
    !rv#  = mm# `or#` (nmm# `and#` (int2Word# (start# +# p#)))
{-# INLINE lineResult# #-}
    

lineSearch :: IntArray s        -- ^ vector to search
           -> Int               -- ^ start index
           -> Int               -- ^ value to search for
           -> ST s Int          -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
lineSearch | wordSize == 32 = lineSearch32
           | otherwise      = lineSearch64
{-# INLINE lineSearch #-}


lineSearch64 :: IntArray s        -- ^ vector to search
             -> Int               -- ^ start index
             -> Int               -- ^ value to search for
             -> ST s Int          -- ^ dest index where it can be found, or
                                  -- \"-1\" if not found
lineSearch64 !vec !start !(I# v#) = do
    (I# x1#) <- M.readArray vec $! start + 0
    let !p1# = maskw# x1# v# `and#` int2Word# 0x1#

    (I# x2#) <- M.readArray vec $! start + 1
    let !p2# = p1# `or#` (maskw# x2# v# `and#` int2Word# 0x2#)

    (I# x3#) <- M.readArray vec $! start + 2
    let !p3# = p2# `or#` (maskw# x3# v# `and#` int2Word# 0x4#)

    (I# x4#) <- M.readArray vec $! start + 3
    let !p4# = p3# `or#` (maskw# x4# v# `and#` int2Word# 0x8#)

    (I# x5#) <- M.readArray vec $! start + 4
    let !p5# = p4# `or#` (maskw# x5# v# `and#` int2Word# 0x10#)

    (I# x6#) <- M.readArray vec $! start + 5
    let !p6# = p5# `or#` (maskw# x6# v# `and#` int2Word# 0x20#)

    (I# x7#) <- M.readArray vec $! start + 6
    let !p7# = p6# `or#` (maskw# x7# v# `and#` int2Word# 0x40#)

    (I# x8#) <- M.readArray vec $! start + 7
    let !p8# = p7# `or#` (maskw# x8# v# `and#` int2Word# 0x80#)

    return $! lineResult# p8# start
{-# INLINE lineSearch64 #-}



lineSearch32 :: IntArray s        -- ^ vector to search
             -> Int               -- ^ start index
             -> Int               -- ^ value to search for
             -> ST s Int          -- ^ dest index where it can be found, or
                                  -- \"-1\" if not found
lineSearch32 !vec !start !(I# v#) = do
    (I# x1#) <- M.readArray vec $! start + 0
    let !p1# = maskw# x1# v# `and#` int2Word# 0x1#

    (I# x2#) <- M.readArray vec $! start + 1
    let !p2# = p1# `or#` (maskw# x2# v# `and#` int2Word# 0x2#)

    (I# x3#) <- M.readArray vec $! start + 2
    let !p3# = p2# `or#` (maskw# x3# v# `and#` int2Word# 0x4#)

    (I# x4#) <- M.readArray vec $! start + 3
    let !p4# = p3# `or#` (maskw# x4# v# `and#` int2Word# 0x8#)

    (I# x5#) <- M.readArray vec $! start + 4
    let !p5# = p4# `or#` (maskw# x5# v# `and#` int2Word# 0x10#)

    (I# x6#) <- M.readArray vec $! start + 5
    let !p6# = p5# `or#` (maskw# x6# v# `and#` int2Word# 0x20#)

    (I# x7#) <- M.readArray vec $! start + 6
    let !p7# = p6# `or#` (maskw# x7# v# `and#` int2Word# 0x40#)

    (I# x8#) <- M.readArray vec $! start + 7
    let !p8# = p7# `or#` (maskw# x8# v# `and#` int2Word# 0x80#)

    (I# x9#) <- M.readArray vec $! start + 8
    let !p9# = p8# `or#` (maskw# x9# v# `and#` int2Word# 0x100#)

    (I# x10#) <- M.readArray vec $! start + 9
    let !p10# = p9# `or#` (maskw# x10# v# `and#` int2Word# 0x200#)

    (I# x11#) <- M.readArray vec $! start + 10
    let !p11# = p10# `or#` (maskw# x11# v# `and#` int2Word# 0x400#)

    (I# x12#) <- M.readArray vec $! start + 11
    let !p12# = p11# `or#` (maskw# x12# v# `and#` int2Word# 0x800#)

    (I# x13#) <- M.readArray vec $! start + 12
    let !p13# = p12# `or#` (maskw# x13# v# `and#` int2Word# 0x1000#)

    (I# x14#) <- M.readArray vec $! start + 13
    let !p14# = p13# `or#` (maskw# x14# v# `and#` int2Word# 0x2000#)

    (I# x15#) <- M.readArray vec $! start + 14
    let !p15# = p14# `or#` (maskw# x15# v# `and#` int2Word# 0x4000#)

    (I# x16#) <- M.readArray vec $! start + 15
    let !p16# = p15# `or#` (maskw# x16# v# `and#` int2Word# 0x8000#)

    return $! lineResult# p16# start
{-# INLINE lineSearch32 #-}

#endif

------------------------------------------------------------------------------
-- | Search through a mutable vector for one of two given int values,
-- cache-line aligned.  If the start index is cache-line aligned, and there is
-- more than a cache-line's room between the start index and the end of the
-- vector, we will search the cache-line all at once using an efficient
-- branchless bit-twiddling technique. Otherwise, we will use a typical loop.
--
cacheLineSearch2 :: IntArray s        -- ^ vector to search
                 -> Int               -- ^ start index
                 -> Int               -- ^ value to search for
                 -> Int               -- ^ value 2 to search for
                 -> ST s Int          -- ^ dest index where it can be found, or
                                     -- \"-1\" if not found
cacheLineSearch2 !vec !start !value !value2 = do
#ifdef NO_C_SEARCH
    let !vlen  = M.length vec
    let !st1   = vlen - start
    let !nvlen = numWordsInCacheLine - st1
    let adv    = (start + cacheLineIntMask) .&. complement cacheLineIntMask
    let st2    = adv - start

    if nvlen > 0 || not (isCacheLineAligned start)
      then naiveSearch2 vec start (min st1 st2) value value2
      else lineSearch2 vec start value value2
#else
    lineSearch2 vec start value value2
#endif
{-# INLINE cacheLineSearch2 #-}


#ifdef NO_C_SEARCH

naiveSearch2 :: IntArray s       -- ^ vector to search
             -> Int              -- ^ start index
             -> Int              -- ^ number of things to search
             -> Int              -- ^ value to search for
             -> Int              -- ^ value 2 to search for
             -> ST s Int         -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
naiveSearch2 !vec !start !nThings !value1 !value2 = go start
  where
    !doneIdx = start + nThings

    go !i | i >= doneIdx = return (-1)
          | otherwise = do
        x <- M.readArray vec i
        if x == value1 || x == value2 then return i else go (i+1)
{-# INLINE naiveSearch2 #-}


lineSearch2 :: IntArray s        -- ^ vector to search
            -> Int               -- ^ start index
            -> Int               -- ^ value to search for
            -> Int               -- ^ value 2 to search for
            -> ST s Int          -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
lineSearch2 | wordSize == 32 = lineSearch32_2
            | otherwise      = lineSearch64_2



lineSearch64_2 :: IntArray s        -- ^ vector to search
               -> Int               -- ^ start index
               -> Int               -- ^ value to search for
               -> Int               -- ^ value 2 to search for
               -> ST s Int          -- ^ dest index where it can be found, or
                                    -- \"-1\" if not found
lineSearch64_2 !vec !start !(I# v#) !(I# v2#) = do
    (I# x1#) <- M.readArray vec $! start + 0
    let !p1# = (maskw# x1# v# `or#` maskw# x1# v2#) `and#` int2Word# 0x1#

    (I# x2#) <- M.readArray vec $! start + 1
    let !p2# = p1# `or#` ((maskw# x2# v# `or#` maskw# x2# v2#)
                          `and#` int2Word# 0x2#)

    (I# x3#) <- M.readArray vec $! start + 2
    let !p3# = p2# `or#` ((maskw# x3# v# `or#` maskw# x3# v2#) 
                          `and#` int2Word# 0x4#)

    (I# x4#) <- M.readArray vec $! start + 3
    let !p4# = p3# `or#` ((maskw# x4# v# `or#` maskw# x4# v2#) 
                          `and#` int2Word# 0x8#)

    (I# x5#) <- M.readArray vec $! start + 4
    let !p5# = p4# `or#` ((maskw# x5# v# `or#` maskw# x5# v2#) 
                          `and#` int2Word# 0x10#)

    (I# x6#) <- M.readArray vec $! start + 5
    let !p6# = p5# `or#` ((maskw# x6# v# `or#` maskw# x6# v2#) 
                          `and#` int2Word# 0x20#)

    (I# x7#) <- M.readArray vec $! start + 6
    let !p7# = p6# `or#` ((maskw# x7# v# `or#` maskw# x7# v2#) 
                          `and#` int2Word# 0x40#)

    (I# x8#) <- M.readArray vec $! start + 7
    let !p8# = p7# `or#` ((maskw# x8# v# `or#` maskw# x8# v2#) 
                          `and#` int2Word# 0x80#)

    return $! lineResult# p8# start
{-# INLINE lineSearch64_2 #-}


lineSearch32_2 :: IntArray s        -- ^ vector to search
               -> Int               -- ^ start index
               -> Int               -- ^ value to search for
               -> Int               -- ^ value 2 to search for
               -> ST s Int          -- ^ dest index where it can be found, or
                                    -- \"-1\" if not found
lineSearch32_2 !vec !start !(I# v#) !(I# v2#) = do
    (I# x1#) <- M.readArray vec $! start + 0
    let !p1# = (maskw# x1# v# `or#` maskw# x1# v2#) `and#` int2Word# 0x1#

    (I# x2#) <- M.readArray vec $! start + 1
    let !p2# = p1# `or#` ((maskw# x2# v# `or#` maskw# x2# v2#) 
                          `and#` int2Word# 0x2#)

    (I# x3#) <- M.readArray vec $! start + 2
    let !p3# = p2# `or#` ((maskw# x3# v# `or#` maskw# x3# v2#) 
                          `and#` int2Word# 0x4#)

    (I# x4#) <- M.readArray vec $! start + 3
    let !p4# = p3# `or#` ((maskw# x4# v# `or#` maskw# x4# v2#) 
                          `and#` int2Word# 0x8#)

    (I# x5#) <- M.readArray vec $! start + 4
    let !p5# = p4# `or#` ((maskw# x5# v# `or#` maskw# x5# v2#) 
                          `and#` int2Word# 0x10#)

    (I# x6#) <- M.readArray vec $! start + 5
    let !p6# = p5# `or#` ((maskw# x6# v# `or#` maskw# x6# v2#) 
                          `and#` int2Word# 0x20#)

    (I# x7#) <- M.readArray vec $! start + 6
    let !p7# = p6# `or#` ((maskw# x7# v# `or#` maskw# x7# v2#) 
                          `and#` int2Word# 0x40#)

    (I# x8#) <- M.readArray vec $! start + 7
    let !p8# = p7# `or#` ((maskw# x8# v# `or#` maskw# x8# v2#) 
                          `and#` int2Word# 0x80#)

    (I# x9#) <- M.readArray vec $! start + 8
    let !p9# = p8# `or#` ((maskw# x9# v# `or#` maskw# x9# v2#) 
                          `and#` int2Word# 0x100#)

    (I# x10#) <- M.readArray vec $! start + 9
    let !p10# = p9# `or#` ((maskw# x10# v# `or#` maskw# x10# v2#) 
                           `and#` int2Word# 0x200#)

    (I# x11#) <- M.readArray vec $! start + 10
    let !p11# = p10# `or#` ((maskw# x11# v# `or#` maskw# x11# v2#) 
                            `and#` int2Word# 0x400#)

    (I# x12#) <- M.readArray vec $! start + 11
    let !p12# = p11# `or#` ((maskw# x12# v# `or#` maskw# x12# v2#) 
                            `and#` int2Word# 0x800#)

    (I# x13#) <- M.readArray vec $! start + 12
    let !p13# = p12# `or#` ((maskw# x13# v# `or#` maskw# x13# v2#) 
                            `and#` int2Word# 0x1000#)

    (I# x14#) <- M.readArray vec $! start + 13
    let !p14# = p13# `or#` ((maskw# x14# v# `or#` maskw# x14# v2#) 
                            `and#` int2Word# 0x2000#)

    (I# x15#) <- M.readArray vec $! start + 14
    let !p15# = p14# `or#` ((maskw# x15# v# `or#` maskw# x15# v2#) 
                            `and#` int2Word# 0x4000#)

    (I# x16#) <- M.readArray vec $! start + 15
    let !p16# = p15# `or#` ((maskw# x16# v# `or#` maskw# x16# v2#) 
                            `and#` int2Word# 0x8000#)

    return $! lineResult# p16# start
{-# INLINE lineSearch32_2 #-}

#endif


------------------------------------------------------------------------------
-- | Search through a mutable vector for one of three given int values,
-- cache-line aligned.  If the start index is cache-line aligned, and there is
-- more than a cache-line's room between the start index and the end of the
-- vector, we will search the cache-line all at once using an efficient
-- branchless bit-twiddling technique. Otherwise, we will use a typical loop.
--
cacheLineSearch3 :: IntArray s        -- ^ vector to search
                 -> Int               -- ^ start index
                 -> Int               -- ^ value to search for
                 -> Int               -- ^ value 2 to search for
                 -> Int               -- ^ value 3 to search for
                 -> ST s Int          -- ^ dest index where it can be found, or
                                     -- \"-1\" if not found
cacheLineSearch3 !vec !start !value !value2 !value3 = do
#ifdef NO_C_SEARCH
    let !vlen  = M.length vec
    let !st1   = vlen - start
    let !nvlen = numWordsInCacheLine - st1
    let adv    = (start + cacheLineIntMask) .&. complement cacheLineIntMask
    let st2    = adv - start

    if nvlen > 0 || not (isCacheLineAligned start)
      then naiveSearch3 vec start (min st1 st2) value value2 value3
      else lineSearch3 vec start value value2 value3
#else
    lineSearch3 vec start value value2 value3
#endif
{-# INLINE cacheLineSearch3 #-}


#ifdef NO_C_SEARCH

naiveSearch3 :: IntArray s       -- ^ vector to search
             -> Int              -- ^ start index
             -> Int              -- ^ number of things to search
             -> Int              -- ^ value to search for
             -> Int              -- ^ value 2 to search for
             -> Int              -- ^ value 3 to search for
             -> ST s Int         -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
naiveSearch3 !vec !start !nThings !value1 !value2 !value3 = go start
  where
    !doneIdx = start + nThings

    go !i | i >= doneIdx = return (-1)
          | otherwise = do
        x <- M.readArray vec i
        if x == value1 || x == value2 || x == value3
          then return i
          else go (i+1)
{-# INLINE naiveSearch3 #-}


lineSearch3 :: IntArray s        -- ^ vector to search
            -> Int               -- ^ start index
            -> Int               -- ^ value to search for
            -> Int               -- ^ value 2 to search for
            -> Int               -- ^ value 3 to search for
            -> ST s Int          -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
lineSearch3 | wordSize == 32 = lineSearch32_3
            | otherwise      = lineSearch64_3



lineSearch64_3 :: IntArray s        -- ^ vector to search
               -> Int               -- ^ start index
               -> Int               -- ^ value to search for
               -> Int               -- ^ value 2 to search for
               -> Int               -- ^ value 3 to search for
               -> ST s Int          -- ^ dest index where it can be found, or
                                    -- \"-1\" if not found
lineSearch64_3 !vec !start !(I# v#) !(I# v2#) !(I# v3#) = do
    (I# x1#) <- M.readArray vec $! start + 0
    let !p1# = (maskw# x1# v# `or#` maskw# x1# v2# `or#` maskw# x1# v3#)
               `and#` int2Word# 0x1#

    (I# x2#) <- M.readArray vec $! start + 1
    let !p2# = p1# `or#`
               ((maskw# x2# v# `or#` maskw# x2# v2# `or#` maskw# x2# v3#)
                `and#` int2Word# 0x2#)

    (I# x3#) <- M.readArray vec $! start + 2
    let !p3# = p2# `or#`
               ((maskw# x3# v# `or#` maskw# x3# v2# `or#` maskw# x3# v3#) 
                `and#` int2Word# 0x4#)

    (I# x4#) <- M.readArray vec $! start + 3
    let !p4# = p3# `or#`
               ((maskw# x4# v# `or#` maskw# x4# v2# `or#` maskw# x4# v3#) 
                `and#` int2Word# 0x8#)

    (I# x5#) <- M.readArray vec $! start + 4
    let !p5# = p4# `or#`
               ((maskw# x5# v# `or#` maskw# x5# v2# `or#` maskw# x5# v3#) 
                `and#` int2Word# 0x10#)

    (I# x6#) <- M.readArray vec $! start + 5
    let !p6# = p5# `or#`
               ((maskw# x6# v# `or#` maskw# x6# v2# `or#` maskw# x6# v3#) 
                `and#` int2Word# 0x20#)

    (I# x7#) <- M.readArray vec $! start + 6
    let !p7# = p6# `or#`
               ((maskw# x7# v# `or#` maskw# x7# v2# `or#` maskw# x7# v3#) 
                `and#` int2Word# 0x40#)

    (I# x8#) <- M.readArray vec $! start + 7
    let !p8# = p7# `or#`
               ((maskw# x8# v# `or#` maskw# x8# v2# `or#` maskw# x8# v3#) 
                `and#` int2Word# 0x80#)

    return $! lineResult# p8# start
{-# INLINE lineSearch64_3 #-}


lineSearch32_3 :: IntArray s        -- ^ vector to search
               -> Int               -- ^ start index
               -> Int               -- ^ value to search for
               -> Int               -- ^ value 2 to search for
               -> Int               -- ^ value 3 to search for
               -> ST s Int          -- ^ dest index where it can be found, or
                                    -- \"-1\" if not found
lineSearch32_3 !vec !start !(I# v#) !(I# v2#) !(I# v3#) = do
    (I# x1#) <- M.readArray vec $! start + 0
    let !p1# = (maskw# x1# v# `or#` maskw# x1# v2# `or#` maskw# x1# v3#)
               `and#` int2Word# 0x1#

    (I# x2#) <- M.readArray vec $! start + 1
    let !p2# = p1# `or#`
               ((maskw# x2# v# `or#` maskw# x2# v2# `or#` maskw# x2# v3#) 
                `and#` int2Word# 0x2#)

    (I# x3#) <- M.readArray vec $! start + 2
    let !p3# = p2# `or#`
               ((maskw# x3# v# `or#` maskw# x3# v2# `or#` maskw# x3# v3#) 
                `and#` int2Word# 0x4#)

    (I# x4#) <- M.readArray vec $! start + 3
    let !p4# = p3# `or#`
               ((maskw# x4# v# `or#` maskw# x4# v2# `or#` maskw# x4# v3#) 
                `and#` int2Word# 0x8#)

    (I# x5#) <- M.readArray vec $! start + 4
    let !p5# = p4# `or#`
               ((maskw# x5# v# `or#` maskw# x5# v2# `or#` maskw# x5# v3#) 
                `and#` int2Word# 0x10#)

    (I# x6#) <- M.readArray vec $! start + 5
    let !p6# = p5# `or#`
               ((maskw# x6# v# `or#` maskw# x6# v2# `or#` maskw# x6# v3#) 
                `and#` int2Word# 0x20#)

    (I# x7#) <- M.readArray vec $! start + 6
    let !p7# = p6# `or#`
               ((maskw# x7# v# `or#` maskw# x7# v2# `or#` maskw# x7# v3#) 
                `and#` int2Word# 0x40#)

    (I# x8#) <- M.readArray vec $! start + 7
    let !p8# = p7# `or#`
               ((maskw# x8# v# `or#` maskw# x8# v2# `or#` maskw# x8# v3#) 
                `and#` int2Word# 0x80#)

    (I# x9#) <- M.readArray vec $! start + 8
    let !p9# = p8# `or#`
               ((maskw# x9# v# `or#` maskw# x9# v2# `or#` maskw# x9# v3#) 
                `and#` int2Word# 0x100#)

    (I# x10#) <- M.readArray vec $! start + 9
    let !p10# = p9# `or#`
                ((maskw# x10# v# `or#` maskw# x10# v2# `or#` maskw# x10# v3#) 
                 `and#` int2Word# 0x200#)

    (I# x11#) <- M.readArray vec $! start + 10
    let !p11# = p10# `or#`
                ((maskw# x11# v# `or#` maskw# x11# v2# `or#` maskw# x11# v3#) 
                 `and#` int2Word# 0x400#)

    (I# x12#) <- M.readArray vec $! start + 11
    let !p12# = p11# `or#`
                ((maskw# x12# v# `or#` maskw# x12# v2# `or#` maskw# x12# v3#) 
                 `and#` int2Word# 0x800#)

    (I# x13#) <- M.readArray vec $! start + 12
    let !p13# = p12# `or#`
                ((maskw# x13# v# `or#` maskw# x13# v2# `or#` maskw# x13# v3#) 
                 `and#` int2Word# 0x1000#)

    (I# x14#) <- M.readArray vec $! start + 13
    let !p14# = p13# `or#`
                ((maskw# x14# v# `or#` maskw# x14# v2# `or#` maskw# x14# v3#) 
                 `and#` int2Word# 0x2000#)

    (I# x15#) <- M.readArray vec $! start + 14
    let !p15# = p14# `or#`
                ((maskw# x15# v# `or#` maskw# x15# v2# `or#` maskw# x15# v3#) 
                 `and#` int2Word# 0x4000#)

    (I# x16#) <- M.readArray vec $! start + 15
    let !p16# = p15# `or#`
                ((maskw# x16# v# `or#` maskw# x16# v2# `or#` maskw# x16# v3#) 
                 `and#` int2Word# 0x8000#)

    return $! lineResult# p16# start
{-# INLINE lineSearch32_3 #-}

#endif
