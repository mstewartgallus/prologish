{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module AsCallByName (Expr, toCbpv, AsAlgebra) where

import Cbpv
import Cbpv.Sort
import Control.Category
import qualified Lambda
import qualified Lambda.Exp as Exp
import qualified Lambda.Product as Product
import qualified Lambda.Sum as Sum
import qualified Lambda.Type as Type
import Prelude hiding ((.), (<*>), id)

newtype Expr c a b = Expr (c (U (AsAlgebra a)) (U (AsAlgebra b)))

type family AsAlgebra a = r where
  AsAlgebra Type.Unit = F Unit
  AsAlgebra Type.Void = F Void
  AsAlgebra (a Type.* b) = F (U (AsAlgebra a) * U (AsAlgebra b))
  AsAlgebra (a Type.+ b) = F (U (AsAlgebra a) + U (AsAlgebra b))
  AsAlgebra (a Type.~> b) = U (AsAlgebra a) ~> AsAlgebra b
  AsAlgebra Type.U64 = F U64

toCbpv :: Cbpv c d => Expr d Type.Unit a -> d (U (F Unit)) (U (AsAlgebra a))
toCbpv (Expr x) = x

instance Cbpv c d => Category (Expr d) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)

instance Cbpv c d => Product.Product (Expr d) where
  unit = Expr (thunk (returns unit))

  first = Expr (thunk (force first . force id))
  second = Expr (thunk (force second . force id))
  Expr f # Expr g = Expr (thunk (returns f `to` ((returns g . returns first) `to` returns ((second . first) # second))))

instance Cbpv c d => Sum.Sum (Expr d) where
  absurd = Expr (thunk (force absurd . force id))

  left = Expr (thunk (returns left))
  right = Expr (thunk (returns right))
  Expr f ! Expr g = Expr (thunk (force id . force (thunk (returns f) ! thunk (returns g)) . force id))

instance Cbpv c d => Exp.Exp (Expr d) where
  lambda (Expr f) = Expr (thunk (lambda (force f . returns (thunk id))))
  Expr f <*> Expr x = Expr (thunk (force f <*> x))

instance Cbpv c d => Lambda.Lambda (Expr d) where
  u64 x = Expr (thunk (returns (u64 x)))
  add = Expr (thunk (addLazy . force id))
