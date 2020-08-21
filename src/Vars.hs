{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Vars (Vars (..), Var (..), eqVar) where

import Data.Typeable ((:~:) (..))
import Product
import Type
import Prelude hiding ((.), id)

class Product k => Vars k where
  mkVar :: Var a -> k x a
  liftVar :: Var a -> k Unit b -> k a b

data Var a = Var (ST a) Int

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing
