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
import HasProduct
import HasSum
import Type
import Prelude hiding (id, (.), (<*>))

class (HasSum t, HasProduct t) => Hoas t where
  -- | set of values such that ...
  st :: ST c -> (t c Void -> t b a) -> t (c -< b) a

  constrain :: t b x -> t (b -< c) x -> t c x

  try :: t (b -< c) x -> t b x -> t c x
  try = flip constrain

  true :: t x B
  false :: t x B

  amb :: t x Void -> t x Void -> t x Void

  isUnit :: t Unit x -> t y x
  isUnit = (. unit)

  isTrue :: t B x -> t y x
  isTrue = (. true)

  -- fixme ... move to HasProduct ?
  isFirst :: t a x -> t (a * b) x
  isFirst = (. first)
  isSecond :: t b x -> t (a * b) x
  isSecond = (. second)

  global :: Global a b -> t a b
  u64 :: Word64 -> t x U64

  succ :: t U64 U64
  succ = global $ Global inferT inferT "core" "succ"

  add :: t (U64 * U64) U64
  add = global $ Global inferT inferT "core" "add"
