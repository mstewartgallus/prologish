{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module AsCbpv (Expr, toCbpv, AsSet) where

import Cbpv
import Cbpv.Sort
import Control.Category
import qualified Lambda
import qualified Lambda.Exp as Exp
import qualified Lambda.Product as Product
import qualified Lambda.Sum as Sum
import qualified Lambda.Type as Type
import Prelude hiding ((.), (<*>), id)

newtype Expr c a b = Expr (c (AsSet a) (AsSet b))

type family AsSet a = r | r -> a where
  AsSet Type.Unit = Unit
  AsSet Type.Void = Void
  AsSet (a Type.* b) = AsSet a * AsSet b
  AsSet (a Type.+ b) = AsSet a + AsSet b
  AsSet (a Type.~> b) = U (AsSet a ~> F (AsSet b))
  AsSet Type.U64 = U64

toCbpv :: Cbpv c d => Expr d Type.Unit a -> d Unit (AsSet a)
toCbpv (Expr x) = x

instance Cbpv c d => Category (Expr d) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)

instance Cbpv c d => Product.Product (Expr d) where
  unit = Expr unit

  first = Expr first
  second = Expr second
  Expr f # Expr g = Expr (f # g)

instance Cbpv c d => Sum.Sum (Expr d) where
  absurd = Expr absurd

  left = Expr left
  right = Expr right
  Expr f ! Expr g = Expr (f ! g)

instance Cbpv c d => Exp.Exp (Expr d) where
  -- lambda (Expr f) = Expr $ lambda (thunk f) . returns id
  -- eval (Expr f) = Expr $ (thunk (eval (force f) id))

instance Cbpv c d => Lambda.Lambda (Expr d) where
  u64 x = Expr (u64 x)
  add = Expr add
