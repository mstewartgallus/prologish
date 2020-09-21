{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Type (inferT, eqT, ST (..), T, Void, Unit, type B, type (+), type (*), type (-<), type U64) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import Type.Reflection

type (-<) = 'Coexp

type (+) = 'Sum

infixr 9 +

type (*) = 'Prod

infixr 9 *

type B = 'B

type U64 = 'U64

type Void = 'Void

type Unit = 'Unit

infixr 9 -<

data T
  = B
  | Void
  | Unit
  | Prod T T
  | Sum T T
  | Coexp T T
  | U64

data ST a where
  SB :: ST B
  SU64 :: ST U64
  SVoid :: ST Void
  SUnit :: ST Unit
  (:+:) :: ST a -> ST b -> ST (a + b)
  (:*:) :: ST a -> ST b -> ST (a * b)
  (:-<) :: ST a -> ST b -> ST (a -< b)

eqT :: ST a -> ST b -> Maybe (a :~: b)
eqT l r = case (l, r) of
  (SU64, SU64) -> Just Refl
  (SB, SB) -> Just Refl
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
    SB -> "b"
    SVoid -> "void"
    SUnit -> "unit"
    x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"
    x :*: y -> "(" ++ show x ++ " * " ++ show y ++ ")"
    x :-< y -> "(" ++ show x ++ " - " ++ show y ++ ")"

inferT :: Typeable a => ST a
inferT = fromTypeRep typeRep

fromTypeRep :: TypeRep a -> ST a
fromTypeRep expr =
  head $
    catMaybes $
      [ do
          HRefl <- eqTypeRep expr (typeRep @U64)
          pure SU64,
        do
          HRefl <- eqTypeRep expr (typeRep @B)
          pure SB,
        do
          HRefl <- eqTypeRep expr (typeRep @Unit)
          pure SUnit,
        do
          HRefl <- eqTypeRep expr (typeRep @Void)
          pure SVoid,
        case expr of
          App (App f x) y -> do
            HRefl <- eqTypeRep (typeRep @'Sum) f
            pure (fromTypeRep x :+: fromTypeRep y)
          _ -> Nothing,
        case expr of
          App (App f x) y -> do
            HRefl <- eqTypeRep (typeRep @'Prod) f
            pure (fromTypeRep x :*: fromTypeRep y)
          _ -> Nothing,
        case expr of
          App (App f x) y -> do
            HRefl <- eqTypeRep (typeRep @'Coexp) f
            pure (fromTypeRep x :-< fromTypeRep y)
          _ -> Nothing
      ]
