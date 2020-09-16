{-# LANGUAGE TypeOperators #-}
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
  data Jump t
  data Case t :: T -> Type
  data Expr t :: T -> Type

  adbmal :: Id -> ST a -> Id -> ST b -> (Case t a -> Expr t b -> Jump t) -> Case t (a -< b)
  try :: Case t (a -< b) -> Case t a -> Case t b

  u64 :: Word64 -> Expr t U64

  unit :: Expr t Unit
  (&&&) :: Expr t a -> Expr t b -> Expr t (a * b)
  first :: Expr t (a * b) -> Expr t a
  second :: Expr t (a * b) -> Expr t b

  empty :: Case t Void
  (|||) :: Case t a -> Case t b -> Case t (a + b)
  left :: Case t (a + b) -> Case t a
  right :: Case t (a + b) -> Case t b

  thunk :: Id -> ST a -> (Case t a -> Jump t) -> Expr t a
  letBe :: Id -> ST a -> (Expr t a -> Jump t) -> Case t a

  jump :: Case t a -> Expr t a -> Jump t

infixr 9 &&&
infixr 9 |||
