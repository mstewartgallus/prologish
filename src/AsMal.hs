{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsMal (Expr, asMal, AsObject) where

import Control.Category
import qualified Hoas.Type as Type
import Mal
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type
import Term (Term)
import qualified Term
import Prelude hiding (id, throw, unthrow, (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.+ b) = AsObject a + AsObject b
  AsObject (a Type.* b) = AsObject a * AsObject b
  AsObject (a Type.-< b) = AsObject a -< AsObject b
  AsObject Type.Void = Void
  AsObject Type.Unit = Unit
  AsObject Type.B = B
  AsObject Type.U64 = U64

type family AsList a = r | r -> a where
  AsList '[] = Unit
  AsList (h ': t) = AsObject h * AsList t

asMal :: Expr k '[] b -> k Unit (AsObject b)
asMal (E x) = x

newtype Expr k a b = E (k (AsList a) (AsObject b))

instance Mal k => Term (Expr k) where
  absurd (E x) = E (absurd . x)

  val (E x) = E (val x)

  -- jump (E f) (E x) = E (jump f x)
  -- tip = E first
  -- const (E x) = E (x . second . x)

  E x `kont` E f = E (kont x (f . (id &&& unit)))

  E f &&& E g = E (f &&& g)
  left (E x) = E (left . x)
  right (E x) = E (right . x)

  first (E x) = E (first . x)
  second (E x) = E (second . x)
  u64 n = E (u64 n . unit)
