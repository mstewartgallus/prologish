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
import qualified Lambda.HasExp as Exp
import qualified Lambda.HasProduct as Product
import qualified Lambda.HasSum as Sum
import qualified Lambda.Type as Type
import Prelude hiding (curry, id, return, uncurry, (.), (<*>))

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

instance Cbpv c d => Product.HasProduct (Expr d) where
  unit = Expr (thunk (return unit))

  first = Expr (thunk (force first . force id))
  second = Expr (thunk (force second . force id))
  Expr f &&& Expr g = Expr (thunk (return f `to` ((return g . return first) `to` return ((second . first) &&& second))))

instance Cbpv c d => Sum.HasSum (Expr d) where
  absurd = Expr (thunk (force absurd . force id))

  left = Expr (thunk (return left))
  right = Expr (thunk (return right))
  Expr f ||| Expr g = Expr (thunk (force id . force (thunk (return f) ||| thunk (return g)) . force id))

instance Cbpv c d => Exp.HasExp (Expr d) where
  curry (Expr f) = Expr (thunk (curry (force f . return (thunk id) . assocOut)))
  uncurry (Expr f) = Expr (thunk (uncurry (force f) . assocIn . force id))

instance Cbpv c d => Lambda.Lambda (Expr d) where
  u64 x = Expr (thunk (return (u64 x)))
  add = Expr (thunk (addLazy . force id))
