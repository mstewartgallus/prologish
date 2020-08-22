{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Product (Product (..)) where

import Control.Category
import Lambda.Type
import Prelude hiding ((.), id)

class Category k => Product k where
  unit :: k x Unit

  (#) :: k env a -> k env b -> k env (a * b)
  first :: k (a * b) a
  second :: k (a * b) b

  letBe :: k env a -> k (env * a) b -> k env b
  letBe = flip whereIs

  whereIs :: k (env * a) b -> k env a -> k env b
  whereIs f x = f . (id # x)

infixl 9 #

infixr 0 `letBe`
