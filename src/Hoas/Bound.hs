{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Data.Word (Word64)

class Bound pos neg | pos -> neg, neg -> pos where
  apply :: neg a -> pos a -> pos c

  kont :: Id -> ST a -> pos x -> (pos a -> pos Void) -> pos (a -< x)
  jump :: Id -> ST x -> pos (a -< x) -> (pos x -> pos a) -> pos c
  val :: pos (a -< x) -> pos x

  unit :: pos Unit
  (&&&) :: pos a -> pos b -> pos (a * b)
  first :: pos (a * b) -> pos a
  second :: pos (a * b) -> pos b

  absurd :: pos Void -> pos a
  left :: pos a -> pos (a + b)
  right :: pos b -> pos (a + b)

  pick :: pos B -> pos (Unit + Unit)
  true :: pos B
  false :: pos B

  u64 :: Word64 -> pos U64
  add :: pos U64 -> pos U64 -> pos U64

  load :: ST a -> String -> pos a
