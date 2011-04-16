{-# LANGUAGE BangPatterns #-}

module Data.HashTable.Internal.CheapPseudoRandomBitStream
  ( BitStream
  , newBitStream
  , getNextBit
  , getNBits
  ) where

import           Control.Applicative
import           Control.Monad.ST
import           Data.Bits
import           Data.Int
import           Data.STRef
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)

import           Data.HashTable.Internal.Utils


------------------------------------------------------------------------------
-- Chosen by fair dice roll. Guaranteed random. More importantly, there are an
-- equal number of 0 and 1 bits in both of these vectors.
random32s :: Vector Int32
random32s = V.fromList [ 0xe293c315
                       , 0x82e2ff62
                       , 0xcb1ef9ae
                       , 0x78850172
                       , 0x551ee1ce
                       , 0x59d6bfd1
                       , 0xb717ec44
                       , 0xe7a3024e
                       , 0x02bb8976
                       , 0x87e2f94f
                       , 0xfa156372
                       , 0xe1325b17
                       , 0xe005642a
                       , 0xc8d02eb3
                       , 0xe90c0a87
                       , 0x4cb9e6e2
                       ]


------------------------------------------------------------------------------
random64s :: Vector Int64
random64s = V.fromList [ 0x62ef447e007e8732
                       , 0x149d6acb499feef8
                       , 0xca7725f9b404fbf8
                       , 0x4b5dfad194e626a9
                       , 0x6d76f2868359491b
                       , 0x6b2284e3645dcc87
                       , 0x5b89b485013eaa16
                       , 0x6e2d4308250c435b
                       , 0xc31e641a659e0013
                       , 0xe237b85e9dc7276d
                       , 0x0b3bb7fa40d94f3f
                       , 0x4da446874d4ca023
                       , 0x69240623fedbd26b
                       , 0x76fb6810dcf894d3
                       , 0xa0da4e0ce57c8ea7
                       , 0xeb76b84453dc3873
                       ]


------------------------------------------------------------------------------
numRandoms :: Int
numRandoms = 16


------------------------------------------------------------------------------
randoms :: Vector Int
randoms | wordSize == 32 = V.map fromEnum random32s
        | otherwise      = V.map fromEnum random64s


------------------------------------------------------------------------------
data BitStream s = BitStream {
      _curRandom :: !(STRef s Int)
    , _bitsLeft  :: !(STRef s Int)
    , _randomPos :: !(STRef s Int)
    }


------------------------------------------------------------------------------
newBitStream :: ST s (BitStream s)
newBitStream =
    unwrapMonad $
    BitStream <$> (WrapMonad $ newSTRef $ V.unsafeIndex randoms 0)
              <*> (WrapMonad $ newSTRef wordSize)
              <*> (WrapMonad $ newSTRef 1)


------------------------------------------------------------------------------
getNextBit :: BitStream s -> ST s Int
getNextBit = getNBits 1


------------------------------------------------------------------------------
getNBits :: Int -> BitStream s -> ST s Int
getNBits nbits (BitStream crRef blRef rpRef) = do
    !bl <- readSTRef blRef
    if bl < nbits
      then newWord
      else nextBits bl

  where
    newWord = do
        !rp <- readSTRef rpRef
        let r = V.unsafeIndex randoms rp
        writeSTRef blRef $! wordSize - nbits
        writeSTRef rpRef $! if rp == (numRandoms-1) then 0 else rp + 1
        extractBits r

    extractBits r = do
        let !b = r .&. ((1 `iShiftL` nbits) - 1)
        writeSTRef crRef $! (r `iShiftRL` nbits)
        return b

    nextBits bl = do
        !r <- readSTRef crRef
        writeSTRef blRef $! bl - nbits
        extractBits r
