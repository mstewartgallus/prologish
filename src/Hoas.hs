{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Control.Category
import Data.Kind
import Data.Word (Word64)
import Global
import Type
import Prelude hiding (id, (.), (<*>))

class Category t => Hoas t where
  -- | set of values such that ...
  st :: ST c -> (t c Void -> t b a) -> t (c -< b) a

  -- | check if the set includes the other set
  try :: t (b -< c) x -> t b x -> t c x

  (|||) :: t x c -> t y c -> t (x + y) c

  global :: Global a b -> t a b
  u64 :: Word64 -> t x U64

  succ :: t U64 U64
  succ = global $ Global inferT inferT "core" "succ"

  add :: t (U64 * U64) U64
  add = global $ Global inferT inferT "core" "add"
