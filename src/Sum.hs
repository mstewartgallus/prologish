{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Sum (Sum (..)) where

import Control.Category
import Data.Kind (Type)
import Type
import Prelude hiding ((.), id)

class Category k => Sum (k :: T -> T -> Type) where
  absurd :: Continuation k Void

  (!) :: k a c -> k b c -> k (a + b) c
  left :: k a (a + b)
  right :: k b (a + b)

  letCase :: k a b -> k env (a + b) -> k env b
  letCase = flip whereCase

  whereCase :: k env (a + b) -> k a b -> k env b
  whereCase f x = (x ! id) . f

infixr 9 !

infixr 0 `letCase`
