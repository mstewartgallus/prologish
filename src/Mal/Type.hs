{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.Type (T, ST (..), Unit, Void, type (-<), type (*), type (+), type B, type U64, eqT) where
import Data.Typeable ((:~:) (..))

type Void = 'Void
type Unit = 'Unit

type (-<) = 'Coexp
infixr 9 -<

type (+) = 'Sum
infixl 0 +

type (*) = 'Prod

infixl 0 *

type B = 'B
type U64 = 'U64

data T = U64 | B | Unit | Void | Sum T T | Prod T T | Coexp T T


data ST a where
  SUnit :: ST Unit
  SVoid :: ST Void
  SB :: ST B
  SU64 :: ST U64
  SCoexp :: ST a -> ST b -> ST (a -< b)
  (:+:) :: ST a -> ST b -> ST (a + b)
  (:*:) :: ST a -> ST b -> ST (a * b)

class KnownT t where
  inferT :: ST t

instance KnownT 'B where
  inferT = SB

instance KnownT 'U64 where
  inferT = SU64

instance KnownT 'Unit where
  inferT = SUnit

instance KnownT 'Void where
  inferT = SVoid

instance (KnownT a, KnownT b) => KnownT ('Coexp a b) where
  inferT = SCoexp inferT inferT

instance (KnownT a, KnownT b) => KnownT ('Sum a b) where
  inferT = inferT :+: inferT

instance (KnownT a, KnownT b) => KnownT ('Prod a b) where
  inferT = inferT :*: inferT

eqT :: ST a -> ST b -> Maybe (a :~: b)
eqT l r = case (l, r) of
  (SVoid, SVoid) -> Just Refl
  (SUnit, SUnit) -> Just Refl
  (SB, SB) -> Just Refl
  (SU64, SU64) -> Just Refl
  (x :+: y, x' :+: y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  (x :*: y, x' :*: y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  (SCoexp x y, SCoexp x' y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  _ -> Nothing

instance Show (ST a) where
  show expr = case expr of
    SVoid -> "void"
    SUnit -> "unit"
    SB -> "b"
    SU64 -> "u64"
    SCoexp x y -> "(" ++ show x ++ " - " ++ show y ++ ")"
    x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"
    x :*: y -> "(" ++ show x ++ " × " ++ show y ++ ")"
