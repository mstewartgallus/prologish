{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Cokappa (..), Cozeta (..), Hoas (..)) where

import Control.Category
import Data.Kind
import Data.Word (Word64)
import Global
import Type
import Prelude hiding (id, (.), (<*>))

-- | Kappa calculus rules
--
-- kappa :: ST c -> (t Unit c -> t a b) -> t (c * a) b
-- lift :: t Unit c -> t a (c * a)
class Category t => Cokappa t where
  label :: ST c -> (t c Void -> t b a) -> t b (c + a)
  lift :: t c Void -> t (c + a) a

-- | Zeta rules
--
-- zeta :: ST c -> (t Unit c -> t a b) -> t a (c ~> b)
-- pass :: t Unit c -> t (c ~> b) b
class Category t => Cozeta t where
  mal :: ST c -> (t c Void -> t b a) -> t (c -< b) a
  pass :: t c Void -> t b (c -< b)

-- |
-- Based on the union of two categories somewhat like Hasegawa.
--
-- Masahito Hasegawa. 1995. Decomposing Typed Lambda Calculus into a
-- Couple of Categorical Programming Languages. In Proceedings of the
-- 6th International Conference on Category Theory and Computer
-- Science (CTCS '95). Springer-Verlag, Berlin, Heidelberg, 200–219.
class (Cokappa t, Cozeta t) => Hoas t where
  global :: Global a b -> t a b
  u64 :: Word64 -> t x U64

  succ :: t U64 U64
  succ = global $ Global inferT inferT "core" "succ"

  add :: t (U64 * U64) U64
  add = global $ Global inferT inferT "core" "add"
