{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Cokappa (..), Cozeta (..), Bound (..)) where

import Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Global
import Data.Kind
import Data.Word (Word64)
import Control.Category

class Category t => Cokappa t where
  label :: Id -> ST c -> (t c Void -> t b a) -> t b (c + a)
  lift :: t c Void -> t (c + a) a

class Category t => Cozeta t where
  mal :: Id -> ST c -> (t c Void -> t b a) -> t (c -< b) a
  pass :: t c Void -> t b (c -< b)

class (Cokappa t, Cozeta t) => Bound t where
  global :: Global a b -> t a b
  u64 :: Word64 -> t x U64
