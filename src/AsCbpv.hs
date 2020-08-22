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
  AsAlgebra Type.Unit = F Unit
  AsAlgebra Type.Void = F Void
  AsAlgebra (a Type.* b) = F (U (AsAlgebra a) * U (AsAlgebra b))
  AsAlgebra (a Type.+ b) = F (U (AsAlgebra a) + U (AsAlgebra b))
  AsAlgebra (a Type.~> b) = U (AsAlgebra a) ~> AsAlgebra b
  AsAlgebra Type.U64 = F U64

toCbpv :: Cbpv c d => Expr c Type.Unit a -> c x (AsAlgebra a)
toCbpv (Expr x) = x . returns unit

instance Cbpv c d => Category (Expr c) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)

instance Cbpv c d => Product.Product (Expr c) where
  unit = Expr (returns unit)

  first = Expr (force first)
  second = Expr (force second)
  Expr f # Expr g = Expr $ returns (thunk (f . force id) # thunk (g . force id))

instance Cbpv c d => Sum.Sum (Expr c) where
  absurd = Expr (force absurd)

  left = Expr (returns left)
  right = Expr (returns right)
  Expr f ! Expr g = Expr $ force (to (returns id . f) ! to (returns id . g))

instance Cbpv c d => Exp.Exp (Expr c) where
  lambda (Expr f) = Expr $ lambda (thunk f) . returns id
  eval (Expr f) = Expr $ force (eval (f . force id))

instance Cbpv c d => Lambda.Lambda (Expr c) where
  u64 x = Expr (force (thunk id . u64 x) . returns unit)
  add = Expr add
