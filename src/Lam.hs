{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lam (Lam (..), Object, Point) where

import Control.Category
import Data.Kind (Type)
import Data.Word (Word64)
import Type
import Prelude hiding ((.), id)

type Object (hom :: T -> T -> Type) a = forall x. hom x a

type Point (hom :: T -> T -> Type) a = forall x. hom a x

class Category k => Lam k where
  unit :: Object k Unit
  absurd :: Point k Void

  (#) :: k env a -> k env b -> k env (a * b)
  first :: k (a * b) a
  second :: k (a * b) b

  letBe :: k env a -> k (env * a) b -> k env b
  letBe = flip whereIs

  whereIs :: k (env * a) b -> k env a -> k env b
  whereIs f x = f . (id # x)

  (!) :: k a c -> k b c -> k (a + b) c
  left :: k a (a + b)
  right :: k b (a + b)

  letCase :: k a b -> k env (a + b) -> k env b
  letCase = flip whereCase

  whereCase :: k env (a + b) -> k a b -> k env b
  whereCase f x = (x ! id) . f

  lambda :: k (env * a) b -> k env (a ~> b)
  unlambda :: k env (a ~> b) -> k (env * a) b

  eval :: k ((a ~> b) * a) b
  eval = unlambda id

  (<*>) :: k env (a ~> b) -> k env a -> k env b
  f <*> x = eval <<< (f # x)

  u64 :: Word64 -> Object k U64
  add :: Object k (U64 ~> U64 ~> U64)

infixr 9 !

infixr 9 #

infixr 0 `letBe`

infixr 0 `letCase`
