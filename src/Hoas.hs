{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Control.Category
import Data.Kind
import Data.Word (Word64)
import Hoas.Global
import Hoas.Type
import Prelude hiding (id, (.), (<*>))

class Category t => Hoas t where
  -- The rule for variable binding is based off the kappa calculus
  -- contextual rule, something like

  -- Kappa calculus rules
  --
  -- kappa :: ST c -> (t Unit c -> t a b) -> t (c * a) b
  -- lift :: t Unit c -> t a (c * a)
  --
  -- Function rules
  --
  -- fn :: ST c -> (t Unit c -> t a b) -> t a (c ~> b)
  -- pass :: t Unit c -> t (c ~> b) b

  -- Dual to the kappa calculus rules
  label :: ST c -> (t c Void -> t b a) -> t b (c + a)
  lift :: t c Void -> t (c + a) a

  mal :: ST c -> (t c Void -> t b a) -> t (c -< b) a
  pass :: t c Void -> t b (c -< b)

  global :: Global a b -> t a b

  u64 :: Word64 -> t x U64

  succ :: t U64 U64
  succ = global $ Global inferT inferT "core" "succ"

  add :: t (U64 * U64) U64
  add = global $ Global inferT inferT "core" "add"
