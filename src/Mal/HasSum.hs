{-# LANGUAGE TypeOperators #-}

module Mal.HasSum (HasSum (..)) where

import Control.Category
import Mal.Type

class Category k => HasSum k where
  absurd :: k Void x

  (|||) :: k a c -> k b c -> k (a + b) c
  left :: k a (a + b)
  right :: k b (a + b)

  commuteSum :: k (a + b) (b + a)
  commuteSum = right ||| left

infixl 9 |||
