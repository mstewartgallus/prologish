{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsCbpv (Expr, toCbpv) where

import Cbpv
import qualified Control.Category as Cat
import Data.Kind
import qualified Product
import Sort
import qualified Type
import Prelude hiding ((.), (<*>), id)

newtype Expr (k :: forall x y. x -> y -> Type) (a :: Type.T) (b :: Type.T) = Expr {unExpr :: k a b}

toCbpv :: forall (k :: forall x y. x -> y -> Type) (a :: Type.T) (b :: Type.T). Expr k a b -> k a b
toCbpv (Expr x) = x

instance forall (k :: forall x y. x -> y -> Type). Cbpv k => Cat.Category (Expr k) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)
