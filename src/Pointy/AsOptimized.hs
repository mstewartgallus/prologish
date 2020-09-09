{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Pointy.AsOptimized () where

import Control.Category
import Lambda.Type
import Data.Word
import Pointy
import Prelude hiding ((.), id, curry, uncurry, Either (..))

-- | The optimizer is based off the idea of normalization by
-- evaluation. Next this should really be moved back to the term level
-- representation
-- optimize :: Lambda k => Expr k a b -> k a b
-- optimize (Expr x) = undefined

newtype Expr t (a :: T) (b :: T) = Expr { apply :: t a -> t b }

data Value k env a where
  StuckValue :: (k env -> k b) -> Value k env a
  EnvValue :: Value k env env

  PointyValue :: (Value k env (b * a) -> Value k env c) -> Value k env a -> Value k env (b ~> c)

  CoinValue :: Value k env Unit
  PairValue :: Value k env a -> Value k env b -> Value k env (a * b)

  LeftValue :: Value k env a -> Value k env (a + b)
  RightValue :: Value k env b -> Value k env (a + b)

compile :: Pointy k => (forall env. Value k env a -> Value k env b) -> k a -> k b
compile f = toPointy (f EnvValue)

toPointy :: Pointy t => Value t a b -> t a -> t b
toPointy expr = case expr of
  EnvValue -> id

  CoinValue -> unit
  PairValue f g -> toPointy f &&& toPointy g

  LeftValue x -> left . toPointy x
  RightValue x -> right . toPointy x

newtype Hom k (a :: T) (b :: T) = Hom { hom :: k a b }

instance Pointy k => Pointy (Value k env) where
  curry = PointyValue

  uncurry f l@(PairValue x env) = case f env of
    PointyValue f a -> f (PairValue x a)

  unit _ = CoinValue

  f &&& g = \x -> PairValue (f x) (g x)
  first (PairValue x _) = x
  second (PairValue _ x) = x

  -- absurd = StuckValue absurd

  f ||| g = \x -> case x of
    LeftValue l -> f l
    RightValue r -> g r
  left = LeftValue
  right = RightValue
