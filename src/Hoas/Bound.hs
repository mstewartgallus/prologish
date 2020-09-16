{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Hoas.Global
import Data.Kind
import Data.Word (Word64)
import Control.Category

class Category t => Bound t where
  letLabel :: Id -> ST a -> (t a Void -> t b r) -> t b (a + r)

  mal :: t b (a + r) -> t (a -< b) r
  try :: t (a -< b) r -> t a r -> t b r

  unit :: t x Unit
  (&&&) :: t x a -> t x b -> t x (a * b)
  first :: t x (a * b) -> t x a
  second :: t x (a * b) -> t x b

  absurd :: t Void r
  (|||) :: t a r -> t b r -> t (a + b) r
  left :: t (a + b) r -> t a r
  right :: t (a + b) r -> t b r

  global :: Global a b -> t a b

  u64 :: Word64 -> t x U64

infixr 9 &&&
infixr 9 |||
