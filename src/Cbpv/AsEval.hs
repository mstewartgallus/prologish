{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv.AsEval (reify, Stack, DataM, Code, Data) where

import Cbpv
import Control.Category
import Data.Word
import Cbpv.Sort
import Prelude hiding ((.), id)

reify :: DataM Unit U64 -> Word64
reify (D f) = case f Unit of
  U64 y -> y

newtype DataM a b = D (Data a -> Data b)

newtype Stack a b = C (Code a -> Code b)

data family Data (a :: Set)

data instance Data (U a) = Thunk (Int -> Code a)

data instance Data Unit = Unit
data instance Data Void
data instance Data (a * b) = Pair { firstOf :: !(Data a), secondOf :: !(Data b) }
data instance Data (a + b) = L !(Data a) | R !(Data b)
newtype instance Data U64 = U64 Word64

data family Code (a :: Algebra)
data instance Code Initial = Effect Int
data instance Code (a & b) = Data a :& Code b
newtype instance Code (a ~> b) = Lam (Data a -> Code b)

instance Category DataM where
  id = D id
  D f . D g = D (f . g)

instance Category Stack where
  id = C id
  C f . C g = C (f . g)

instance Cbpv Stack DataM where
  to (C f) (C g) = C $ \x@(env :& _) -> case f x of
    (y :& k) -> g ((Pair env y) :& k)
  returns (D f) = C $ \(x :& w) -> f x :& w

  thunk (C f) = D $ \x -> Thunk $ \w -> f (x :& Effect w)
  force (D f) = C $ \(x :& Effect w) -> case f x of
    Thunk t -> t w

  absurd = D $ \x -> case x of {}
  D x ! D y = D $ \env -> case env of
    L l -> x l
    R r -> y r
  left = D L
  right = D R

  unit = D $ const Unit
  D x # D y = D $ \env -> Pair (x env) (y env)
  first = D firstOf
  second = D secondOf

  lambda (C f) = C $ \(env :& w) -> Lam $ \x -> f ((Pair env x) :& w)
  eval (C f) (D x) = C $ \(env :& w) -> case f (env :& w) of
     Lam y -> y (x env)

  u64 x = D $ const (U64 x)
  -- add = C $ \(Effect w0 Unit) ->
  --   Lam $ \(Thunk x) ->
  --   Lam $ \(Thunk y) ->
  --   case x w0 of
  --     Effect w1 (U64 x') ->
  --       case y w1 of
  --         Effect w2 (U64 y') ->
  --             Effect w2 (U64 (x' + y'))
