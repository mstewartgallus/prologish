{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Sort
  ( Set,
    U,
    Unit,
    type (*),
    type (+),
    U64,
    SSet (..),
    KnownSet,
    inferSet,
    eqSet,
    Algebra,
    F,
    Void,
    type (~>),
    SAlgebra (..),
    KnownAlgebra,
    inferAlgebra,
    eqAlgebra,
  )
where

import Data.Kind (Type)
import Data.Typeable ((:~:) (..))

type Set = SetImpl

type U = 'U

type Unit = 'Unit

type (*) = 'Product

type (+) = 'Sum

type U64 = 'U64

type Algebra = AlgebraImpl

type F = 'F

type Void = 'Void

type (~>) = 'Exp

data SetImpl = U Algebra | Unit | Sum Set Set | Product Set Set | U64

data AlgebraImpl = F Set | Void | Exp Set Algebra

infixr 9 ~>

infixl 0 *

infixl 0 +

data SSet a where
  SU :: SAlgebra a -> SSet (U a)
  SUnit :: SSet Unit
  SU64 :: SSet U64
  (:*:) :: SSet a -> SSet b -> SSet (a * b)
  (:+:) :: SSet a -> SSet b -> SSet (a + b)

data SAlgebra a where
  SF :: SSet a -> SAlgebra (F a)
  SVoid :: SAlgebra Void
  (:->) :: SSet a -> SAlgebra b -> SAlgebra (a ~> b)

class KnownSet t where
  inferSet :: SSet t

class KnownAlgebra t where
  inferAlgebra :: SAlgebra t

instance KnownAlgebra a => KnownSet ('U a) where
  inferSet = SU inferAlgebra

instance KnownSet 'Unit where
  inferSet = SUnit

instance (KnownSet a, KnownSet b) => KnownSet ('Product a b) where
  inferSet = inferSet :*: inferSet

instance (KnownSet a, KnownSet b) => KnownSet ('Sum a b) where
  inferSet = inferSet :+: inferSet

instance KnownSet 'U64 where
  inferSet = SU64

instance KnownSet a => KnownAlgebra ('F a) where
  inferAlgebra = SF inferSet

instance KnownAlgebra 'Void where
  inferAlgebra = SVoid

instance (KnownSet a, KnownAlgebra b) => KnownAlgebra ('Exp a b) where
  inferAlgebra = inferSet :-> inferAlgebra

eqAlgebra :: SAlgebra a -> SAlgebra b -> Maybe (a :~: b)
eqAlgebra x y = case (x, y) of
  (SF x, SF x') -> case eqSet x x' of
    Just Refl -> Just Refl
    _ -> Nothing
  (SVoid, SVoid) -> Just Refl
  (a :-> b, a' :-> b') -> case (eqSet a a', eqAlgebra b b') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  _ -> Nothing

eqSet :: SSet a -> SSet b -> Maybe (a :~: b)
eqSet x y = case (x, y) of
  (SU x, SU x') -> case eqAlgebra x x' of
    Just Refl -> Just Refl
    _ -> Nothing
  (SUnit, SUnit) -> Just Refl
  (a :*: b, a' :*: b') -> case (eqSet a a', eqSet b b') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  (a :+: b, a' :+: b') -> case (eqSet a a', eqSet b b') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  (SU64, SU64) -> Just Refl
  _ -> Nothing

instance Show (SSet a) where
  show expr = case expr of
    SU x -> "(U " ++ show x ++ ")"
    SUnit -> "Unit"
    SU64 -> "U64"
    x :*: y -> "(" ++ show x ++ " * " ++ show y ++ ")"
    x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"

instance Show (SAlgebra a) where
  show expr = case expr of
    SF x -> "(F " ++ show x ++ ")"
    SVoid -> "Void"
    x :-> y -> "(" ++ show x ++ " ~> " ++ show y ++ ")"
