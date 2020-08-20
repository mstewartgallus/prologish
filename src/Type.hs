{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Type (T, Void, Unit, type (~>), type (*), type (+), U64, ST (..), KnownT, inferT, eqT, Value, Continuation) where

import Data.Kind (Type)
import Data.Typeable ((:~:) (..))

type Value (hom :: T -> T -> Type) a = forall x. hom x a

type Continuation (hom :: T -> T -> Type) a = forall x. hom a x

type T = TypeImpl

type Void = 'Void

type Unit = 'Unit

type (~>) = 'Exp

type (*) = 'Product

type (+) = 'Sum

type U64 = 'U64

data TypeImpl = Void | Unit | Exp T T | Product T T | Sum T T | U64

infixr 9 ~>

infixr 0 *

infixr 0 +

data ST a where
  SUnit :: ST Unit
  SVoid :: ST Void
  SU64 :: ST U64
  (:->) :: ST a -> ST b -> ST (a ~> b)
  (:*:) :: ST a -> ST b -> ST (a * b)
  (:+:) :: ST a -> ST b -> ST (a + b)

class KnownT t where
  inferT :: ST t

instance KnownT 'Unit where
  inferT = SUnit

instance KnownT 'Void where
  inferT = SVoid

instance KnownT 'U64 where
  inferT = SU64

instance (KnownT a, KnownT b) => KnownT (Exp a b) where
  inferT = inferT :-> inferT

instance (KnownT a, KnownT b) => KnownT (Product a b) where
  inferT = inferT :*: inferT

instance (KnownT a, KnownT b) => KnownT (Sum a b) where
  inferT = inferT :+: inferT

eqT :: ST a -> ST b -> Maybe (a :~: b)
eqT x y = case (x, y) of
  (SUnit, SUnit) -> Just Refl
  (SU64, SU64) -> Just Refl
  (a :-> b, a' :-> b') -> case (eqT a a', eqT b b') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  (a :*: b, a' :*: b') -> case (eqT a a', eqT b b') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  (a :+: b, a' :+: b') -> case (eqT a a', eqT b b') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing

instance Show (ST a) where
  show expr = case expr of
    SUnit -> "Unit"
    SVoid -> "Void"
    SU64 -> "U64"
    x :-> y -> "(" ++ show x ++ " ~> " ++ show y ++ ")"
    x :*: y -> "(" ++ show x ++ " * " ++ show y ++ ")"
    x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"
