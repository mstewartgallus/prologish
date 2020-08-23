{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module AsCbpv (Expr, toCbpv, AsSet, foo) where

import Cbpv
import Cbpv.Sort
import Control.Category
import qualified Lambda
import qualified Lambda.Exp as Exp
import qualified Lambda.Product as Product
import qualified Lambda.Sum as Sum
import qualified Lambda.Type as Type
import Prelude hiding ((.), (<*>), id)

newtype Expr c a b = Expr (c (F (AsSet a)) (F (AsSet b)))

type family AsSet a = r | r -> a where
  AsSet Type.Unit = Unit
  AsSet Type.Void = Void
  AsSet (a Type.* b) = AsSet a * AsSet b
  AsSet (a Type.+ b) = AsSet a + AsSet b
  AsSet (a Type.~> b) = U (AsSet a ~> F (AsSet b))
  AsSet Type.U64 = U64

toCbpv :: Cbpv c d => Expr c Type.Unit a -> c (F Unit) (F (AsSet a))
toCbpv (Expr x) = x

instance Cbpv c d => Category (Expr c) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)

instance Cbpv c d => Product.Product (Expr c) where
  unit = Expr (returns unit)

  first = Expr (returns first)
  second = Expr (returns second)
  Expr f # Expr g = Expr (f `to` ((g . returns first) `to` returns ((second . first) # second)))

instance Cbpv c d => Sum.Sum (Expr c) where
  absurd = Expr (returns absurd)

  left = Expr (returns left)
  right = Expr (returns right)
  Expr f ! Expr g = Expr (force (thunk f ! thunk g))

instance Cbpv c d => Exp.Exp (Expr c)

-- lambda (Expr f) = Expr $ lambda (thunk f) . returns id
-- Expr f <*> Expr x = Expr (x `to` ((f . returns first) <*> second))

instance Cbpv c d => Lambda.Lambda (Expr c) where
  u64 x = Expr (returns (u64 x))
  add = Expr (returns add)

-- foo f x = thunk (force f <*> x)
foo f g = force (thunk f ! thunk g)
