{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv.AsEval (reify, Action, Data) where

import Cbpv
import Control.Category
import Data.Word
import Cbpv.Sort
import Prelude hiding ((.), id)

reify :: (Data (U (F Unit)) -> Data (U (F U64))) -> Word64
reify f = case f (Thunk $ \w -> Unit :& Effect w) of
  Thunk t -> case t 0 of
    (U64 y :& _) -> y

data family Data (a :: Set)

data instance Data (U a) = Thunk (Int -> Action a)

data instance Data Unit = Unit
data instance Data Void
data instance Data (a * b) = Pair { firstOf :: (Data a), secondOf :: (Data b) }
data instance Data (a + b) = L (Data a) | R (Data b)
newtype instance Data U64 = U64 Word64

-- | Actions are CBPVs computations but we use a different name for brevity
data family Action (a :: Algebra)
data instance Action Initial = Effect Int
data instance Action (a & b) = Data a :& Action b
newtype instance Action (a ~> b) = Lam (Data a -> Action b)

instance Cbpv Action Data where
  to f g = \x@(env :& _) -> case f x of
    (y :& k) -> g ((Pair env y) :& k)
  return f = \(x :& w) -> f x :& w

  thunk (f) = \x -> Thunk $ \w -> f (x :& Effect w)
  force (f) = \(x :& Effect w) -> case f x of
    Thunk t -> t w

  absurd =  \x -> case x of {}
  x ||| y = \env -> case env of
    L l -> x l
    R r -> y r
  left = L
  right = R

  unit = const Unit
  x &&& y = \env -> Pair (x env) (y env)
  first = firstOf
  second = secondOf

  curry (f) = \env -> Lam $ \x -> f (x :& env)
  uncurry (f) = \(x :& env) -> case f env of
     Lam y -> y x

  assocOut = \(a :& (b :& c)) -> (Pair a b) :& c
  assocIn = \((Pair a b) :& c) -> a :& (b :& c)

  u64 x = const (U64 x)
  add = \Unit ->
    Thunk $ \w0 ->
    Lam $ \(U64 x) ->
    (:& Effect w0) $ Thunk $ \w1 ->
    Lam $ \(U64 y) ->
    U64 (x + y) :& Effect w1

  addLazy = \(Unit :& Effect w0) ->
    Lam $ \(Thunk x) -> Lam $ \(Thunk y) -> case x w0 of
       U64 x' :& Effect w1 -> case y w1 of
         U64 y' :& Effect w2 -> U64 (x' + y') :& Effect w2
