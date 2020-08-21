{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module AsCbpv (Expr, toCbpv, AsAlgebra) where

import Cbpv
import Control.Category
import qualified Exp
import qualified Lambda
import qualified Product
import Sort
import qualified Sum
import qualified Type
import Prelude hiding ((.), (<*>), id)

newtype Expr c a b = Expr (c (AsAlgebra a) (AsAlgebra b))

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

  first = undefined
  second = undefined
  (#) = undefined

instance Cbpv c d => Sum.Sum (Expr c) where
  absurd = Expr absurd

  left = undefined
  right = undefined
  (!) = undefined

instance Cbpv c d => Exp.Exp (Expr c) where
  lambda = undefined
  eval = undefined

instance Cbpv c d => Lambda.Lambda (Expr c) where
  u64 = undefined
  add = undefined
