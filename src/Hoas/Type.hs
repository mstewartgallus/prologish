{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Type (KnownT, inferT, eqT, ST (..), T, Void, Unit, type (+), type (*), type (-<), type U64) where

import Data.Typeable ((:~:) (..))

type (-<) = 'Coexp
type (+) = 'Sum
infixr 9 +
type (*) = 'Prod
infixr 9 *

type U64 = 'U64
type Void = 'Void
type Unit = 'Unit

infixr 9 -<

data T = U64 | Void | Unit | Prod T T | Sum T T | Coexp T T

data ST a where
  SU64 :: ST U64
  SVoid :: ST Void
  SUnit :: ST Unit
  (:+:) :: ST a -> ST b -> ST (a + b)
  (:*:) :: ST a -> ST b -> ST (a * b)
  (:-<) :: ST a -> ST b -> ST (a -< b)

eqT :: ST a -> ST b -> Maybe (a :~: b)
eqT l r = case (l, r) of
  (SU64, SU64) -> Just Refl
  (SVoid, SVoid) -> Just Refl
  (SUnit, SUnit) -> Just Refl
  (x :*: y, x' :*: y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  (x :+: y, x' :+: y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  (x :-< y, x' :-< y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  _ -> Nothing

instance Show (ST a) where
  show expr = case expr of
    SU64 -> "u64"
    SVoid -> "void"
    SUnit -> "unit"
    x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"
    x :*: y -> "(" ++ show x ++ " * " ++ show y ++ ")"
    x :-< y -> "(" ++ show x ++ " - " ++ show y ++ ")"

class KnownT t where
  inferT :: ST t

instance KnownT 'U64 where
  inferT = SU64

instance KnownT 'Unit where
  inferT = SUnit
instance KnownT 'Void where
  inferT = SVoid

instance (KnownT a, KnownT b) => KnownT ('Sum a b) where
  inferT = inferT :+: inferT

instance (KnownT a, KnownT b) => KnownT ('Prod a b) where
  inferT = inferT :*: inferT

instance (KnownT a, KnownT b) => KnownT ('Coexp a b) where
  inferT = inferT :-< inferT

