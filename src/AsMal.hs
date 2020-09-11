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
  AsObject Type.U64 = U64

type family AsList a = r | r -> a where
  AsList '[] = Void
  AsList (h ': t) = AsObject h + AsList t

asMal :: Expr k b '[] -> k (AsObject b) Void
asMal (E x) = x

newtype Expr k b a = E (k (AsObject b) (AsList a))

instance Mal k => Term (Expr k) where
  absurd = E absurd
  tip = E left
  const (E x) = E (right . x)

  mal (E f) = E (mal f)
  E f `try` E x = E (f `tryCatch` x)

  E x `isBoth` (E f, E g) =
    E $
      let f' = mal ((right ||| left) . try f)
          g' = mal ((right ||| left) . try g)
       in (id ||| x) . try (f' &&& g')

  isFirst (E x) = E (x . first)
  isSecond (E x) = E (x . second)
  E x `isU64` n = E (x . u64 n)

-- add = E (absurd . add)
