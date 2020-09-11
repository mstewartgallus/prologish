{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Type (KnownT, inferT, eqT, ST (..), T, Void, Unit, type (+), type (*), type (-<), type U64) where

import Data.Typeable ((:~:) (..))

type (-<) = 'Coexp

infixr 9 -<

type (*) = 'Prod
infixl 0 *

type (+) = 'Sum

infixl 0 +

type Void = 'Void
type Unit = 'Unit
type U64 = 'U64

data T = Unit | Void | Sum T T | Prod T T | Coexp T T
  | U64

data ST a where
  SUnit :: ST Unit
  SVoid :: ST Void
  SU64 :: ST U64
  (:+:) :: ST a -> ST b -> ST (a + b)
  (:*:) :: ST a -> ST b -> ST (a * b)
  (:-<) :: ST a -> ST b -> ST (a -< b)

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

eqT :: ST a -> ST b -> Maybe (a :~: b)
eqT l r = case (l, r) of
  (SVoid, SVoid) -> Just Refl
  (SUnit, SUnit) -> Just Refl
  (SU64, SU64) -> Just Refl
  (x :+: y, x' :+: y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  (x :*: y, x' :*: y') -> do
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
    SVoid -> "void"
    SUnit -> "unit"
    SU64 -> "u64"
    x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"
    x :*: y -> "(" ++ show x ++ " * " ++ show y ++ ")"
    x :-< y -> "(" ++ show x ++ " -< " ++ show y ++ ")"
