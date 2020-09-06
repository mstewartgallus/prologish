{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HasWord (HasWordType (..), HasWord (..)) where

import Data.Word (Word64)

class HasWordType k where
  type U64 k :: k

class HasWordType k => HasWord (t :: k -> *) where
  u64 :: Word64 -> t (U64 k)
