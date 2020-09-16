{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Control.Category
import Data.Kind
import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (id, (.), (<*>))

class Category t => Hoas t where
  -- The rule for variable binding is based off the kappa calculus
  -- contextual rule, something like
  --
  -- kappa :: ST a -> (t Unit a -> t b c) -> t (a * b) c
  -- Let label is dual to the kappa calculus rule...
  letLabel :: ST a -> (t a Void -> t b r) -> t b (a + r)

  -- Dual rules to the exponential rules
  mal :: t b (a + r) -> t (a -< b) r
  try :: t (a -< b) r -> t a r -> t b r

  unit :: t x Unit
  (&&&) :: t x a -> t x b -> t x (a * b)
  first :: t x (a * b) -> t x a
  second :: t x (a * b) -> t x b

  -- By having a domain as well as a codomain we can present sum types
  -- dually as sort of a pattern matching language.
  absurd :: t Void r
  (|||) :: t a r -> t b r -> t (a + b) r
  left :: t (a + b) r -> t a r
  right :: t (a + b) r -> t b r

  u64 :: Word64 -> t x U64
  add :: t x U64 -> t x U64 -> t x U64

infixr 9 &&&

infixr 9 |||
