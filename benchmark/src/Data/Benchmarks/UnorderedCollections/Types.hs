{-# LANGUAGE BangPatterns #-}

module Data.Benchmarks.UnorderedCollections.Types
  ( Operation(..)
  ) where

import           Control.DeepSeq


------------------------------------------------------------------------------
data Operation k = 
      -- | Insert a k-v pair into the collection. If k existed, we should
      -- update the mapping.
      Insert {-# UNPACK #-} !k
             {-# UNPACK #-} !Int
      -- | Lookup a key in the mapping.
    | Lookup {-# UNPACK #-} !k
      -- | Delete a key from the mapping.
    | Delete {-# UNPACK #-} !k
  deriving (Show)


------------------------------------------------------------------------------
instance (NFData k) => NFData (Operation k) where
    rnf (Insert k v) = rnf k `seq` rnf v
    rnf (Lookup k)   = rnf k
    rnf (Delete k)   = rnf k
