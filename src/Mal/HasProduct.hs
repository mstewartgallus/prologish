{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.HasProduct (HasProduct (..)) where

import Control.Category
import Mal.Type

class Category k => HasProduct k where
  unit :: k x Unit

  (&&&) :: k c a -> k c b -> k c (a * b)
  first :: k (a * b) a
  second :: k (a * b) b

infixl 9 &&&
