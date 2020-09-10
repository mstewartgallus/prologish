{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Type (KnownT, inferT, eqT, ST (..), T, type (-<), type U64) where

import Data.Typeable ((:~:) (..))

type (-<) = 'Coexp

infixr 9 -<

type U64 = 'U64

data T = U64 | Coexp T T

data ST a where
  SU64 :: ST U64
  (:-<) :: ST a -> ST b -> ST (a -< b)

class KnownT t where
  inferT :: ST t

instance KnownT 'U64 where
  inferT = SU64

instance (KnownT a, KnownT b) => KnownT ('Coexp a b) where
  inferT = inferT :-< inferT

eqT :: ST a -> ST b -> Maybe (a :~: b)
eqT l r = case (l, r) of
  (SU64, SU64) -> Just Refl
  (x :-< y, x' :-< y') -> do
    Refl <- eqT x x'
    Refl <- eqT y y'
    return Refl
  _ -> Nothing

instance Show (ST a) where
  show expr = case expr of
    SU64 -> "u64"
    x :-< y -> "(" ++ show x ++ " -< " ++ show y ++ ")"
