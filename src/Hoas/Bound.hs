{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Global
import Data.Kind
import Data.Word (Word64)
import Control.Category

class Category t => Bound t where
  st :: Id -> ST c -> (t c Void -> t b a) -> t (c -< b) a
  try :: t (b -< c) x -> t b x -> t c x

  (|||) :: t a x -> t b x -> t (a + b) x

  global :: Global a b -> t a b
  u64 :: Word64 -> t x U64
