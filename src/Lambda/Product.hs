{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Product (Product (..)) where

import Control.Category
import Lambda.Type

class Category k => Product k where
  unit :: k x Unit

  (&&&) :: k env a -> k env b -> k env (a * b)
  first :: k (a * b) a
  second :: k (a * b) b

infixl 9 &&&
