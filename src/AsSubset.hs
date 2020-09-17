{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- | Strict / Strict Data are not for performance but simply to
-- emphasize the semantics don't depend on laziness
module AsSubset (Expr, asSubset, Value (..)) where

import Control.Applicative
import Control.Category
import Data.Kind
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Void as Void
import Data.Word
import Global
import HasCoexp
import HasProduct
import HasSum
import Mal
import Type
import Prelude hiding (Bool (..), Either (..), id, (.))

asSubset :: Expr a b -> Value b -> Value a
asSubset (E x) = x

data Value (a :: T) where
  EmptySet :: Value a
  First :: Value a -> Value (a * b)
  Second :: Value b -> Value (a * b)
  Or :: Value a -> Value b -> Value (a + b)
  CoinSet :: Value Unit
  Value64 :: Set Word64 -> Value U64
  ValueCoexp :: (Value a -> Value b) -> Value (a -< b)

newtype Expr a b = E (Value b -> Value a)

instance Category Expr where
  id = E id
  E f . E g = E (g . f)

instance HasProduct Expr where
  unit = E $ const $ EmptySet

  -- FIXME Doesn't make sense to me at all
  E f &&& E g = E $ \x -> case x of
    EmptySet -> EmptySet
    First l -> f l
    Second r -> g r
  first = E First
  second = E Second

instance HasSum Expr where
  absurd = E $ \EmptySet -> EmptySet

  E f ||| E g = E $ \x -> Or (f x) (g x)
  left = E $ \x -> case x of
    EmptySet -> EmptySet
    Or l _ -> l
  right = E $ \x -> case x of
    EmptySet -> EmptySet
    Or _ r -> r

instance HasCoexp Expr where
  mal (E f) = E $ \x -> ValueCoexp $ \y -> f (Or y x)

  try (E f) = E $ \x ->
    let (l', r') = case x of
          EmptySet -> (EmptySet, EmptySet)
          Or l r -> (l, r)
     in case f r' of
          EmptySet -> EmptySet
          ValueCoexp g -> g l'

instance Mal Expr where
  u64 x = E $ \y -> case y of
    EmptySet -> EmptySet
    Value64 y' -> if x `Set.member` y' then CoinSet else EmptySet

  global (Global SU64 SU64 "core" "succ") = E $ \expr -> case expr of
    EmptySet -> EmptySet
    Value64 x -> Value64 ((Set.fromList . map (\n -> n - 1) . Set.toList) x)
