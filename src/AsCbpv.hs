{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module AsCbpv (Expr, toCbpv, AsAlgebra) where

import Cbpv
import Control.Category
import Data.Kind
import qualified Exp
import qualified Lambda
import qualified Product
import Sort
import qualified Sum
import qualified Type
import Prelude hiding ((.), (<*>), id)

newtype Expr c (a :: Type.T) (b :: Type.T) = Expr {unExpr :: c (AsAlgebra a) (AsAlgebra b)}

type family AsAlgebra a where
  AsAlgebra Type.Unit = Initial
  AsAlgebra Type.Void = Void
  AsAlgebra (a Type.* b) = F (U (AsAlgebra a) * U (AsAlgebra b))
  AsAlgebra (a Type.+ b) = F (U (AsAlgebra a) + U (AsAlgebra b))
  AsAlgebra (a Type.~> b) = U (AsAlgebra a) ~> AsAlgebra b
  AsAlgebra Type.U64 = F U64

toCbpv :: Cbpv c d => Expr c Type.Unit a -> c x (AsAlgebra a)
toCbpv (Expr x) = x . initial

instance Cbpv c d => Category (Expr c) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)

instance Cbpv c d => Product.Product (Expr c) where
  unit = Expr initial

instance Cbpv c d => Sum.Sum (Expr c) where
  absurd = Expr absurd

-- left = Expr (force left . unbox)
-- right = Expr (mapCodeR right thunk)
-- Expr f ! Expr g = Expr (mapCodeR (mapDataR f force ! mapDataR g force) thunk)

instance Cbpv c d => Exp.Exp (Expr c)

instance Cbpv c d => Lambda.Lambda (Expr c)
