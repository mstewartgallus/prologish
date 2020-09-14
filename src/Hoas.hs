{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (uncurry, (.), (<*>))

class Hoas pos neg | pos -> neg, neg -> pos where
  apply :: neg a -> pos a -> pos c

  -- Basic higher order CPS combinators
  kont :: ST a -> pos x -> (pos a -> pos Void) -> pos (a -< x)
  jump :: ST x -> pos (a -< x) -> (pos x -> pos a) -> pos c
  val :: pos (a -< x) -> pos x

  unit :: pos Unit
  (&&&) :: pos a -> pos b -> pos (a * b)
  first :: pos (a * b) -> pos a
  second :: pos (a * b) -> pos b

  absurd :: pos Void -> pos a
  either :: pos (a + b) -> (pos a -> pos c, pos b -> pos c) -> pos c
  left :: pos a -> pos (a + b)
  right :: pos b -> pos (a + b)

  pick :: pos B -> pos (Unit + Unit)
  true :: pos B
  false :: pos B

  u64 :: Word64 -> pos U64
  add :: pos U64 -> pos U64 -> pos U64

  load :: ST a -> String -> pos a

  -- | Syntactic sugar for easier programming
  (\+) :: KnownT a => pos x -> (pos a -> pos Void) -> pos (a -< x)
  x \+ k = kont inferT x k

  fn :: (KnownT a, KnownT b) => (pos b -> pos a) -> pos (K (a -< b))
  fn f = unit \+ \x -> x ! f

  (!) :: KnownT x => pos (a -< x) -> (pos x -> pos a) -> pos c
  x ! k = jump inferT x k

infixl 9 &&&

infixl 0 \+

infixl 0 !
