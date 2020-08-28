{-# LANGUAGE TypeOperators #-}

module Lambda.Sum (Sum (..)) where

import Control.Category
import Lambda.Type

class Category k => Sum k where
  absurd :: k Void x

  (|||) :: k a c -> k b c -> k (a + b) c
  left :: k a (a + b)
  right :: k b (a + b)

infixl 9 |||
