{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Product (Product (..)) where

import Control.Category
import Data.Kind (Type)
import Type
import Prelude hiding ((.), id)

class Category k => Product (k :: T -> T -> Type) where
  unit :: Value k Unit
  (#) :: k env a -> k env b -> k env (a * b)
  first :: k (a * b) a
  second :: k (a * b) b

  letBe :: k env a -> k (env * a) b -> k env b
  letBe = flip whereIs

  whereIs :: k (env * a) b -> k env a -> k env b
  whereIs f x = f . (id # x)

infixl 9 #

infixr 0 `letBe`
