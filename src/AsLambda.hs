{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsLambda (Expr, asLambda, AsObject) where

import Control.Category
import qualified Hoas.Type as Type
import Lambda
import Lambda.HasExp
import Lambda.HasProduct
import Lambda.Type
import Term (Term)
import qualified Term
import Prelude hiding (curry, id, uncurry, (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.~> b) = AsObject a ~> AsObject b
  AsObject Type.U64 = U64

type family AsList a = r | r -> a where
  AsList '[] = Unit
  AsList (h ': t) = AsObject h * AsList t

asLambda :: Expr k '[] b -> k Unit (AsObject b)
asLambda (E x) = x

data Expr k (a :: [Type.T]) (b :: Type.T) where
  E :: k (AsList a) (AsObject b) -> Expr k a b

instance Lambda k => Term (Expr k) where
  tip = E first
  const (E x) = E (x . second)

  E x `be` E f = E (curry f <*> x)

  curry (E f) = E (curry f)
  E f <*> E x = E (f <*> x)

  u64 x = E (u64 x . unit)
  add = E (add . unit)
