{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.Type (ST (..), eqT, T, Void, Unit, type (-<), type (*), type (+), type U64) where
import Data.Typeable ((:~:)(..))

type Void = 'Void

type Unit = 'Unit

type (-<) = 'Coexp

type (*) = 'Product

type (+) = 'Sum

type U64 = 'U64

infixr 9 -<

infixl 0 *

infixl 0 +

data T = U64 | Void | Unit | Sum T T | Product T T | Coexp T T

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
