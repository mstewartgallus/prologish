{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Data.Kind
import Data.Word (Word64)

class Bound t where
  mal :: Id -> ST a -> (t a r -> t b r) -> t (a -< b) r
  try :: t (a -< b) r -> t a r -> t b r

  unit :: t x Unit
  (&&&) :: t x a -> t x b -> t x (a * b)
  first :: t x (a * b) -> t x a
  second :: t x (a * b) -> t x b

  empty :: t Void r
  (|||) :: t a r -> t b r -> t (a + b) r
  left :: t (a + b) r -> t a r
  right :: t (a + b) r -> t b r

  jump :: t a r -> t x a -> t x r
  thunk :: Id -> ST a -> (forall r. t a r -> t x r) -> t x a
  letBe :: Id -> ST a -> (forall x. t x a -> t x r) -> t a r

  u64 :: Word64 -> t x U64

infixr 9 &&&
infixr 9 |||
