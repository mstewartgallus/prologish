{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsMal (Expr, asMal, AsOp) where

import Control.Category
import Lambda (Lambda)
import qualified Lambda
import Lambda.HasExp
import Lambda.HasProduct
import qualified Lambda.Type as Type
import Mal
import Mal.HasCoexp
import Mal.HasSum
import Mal.Type
import Prelude hiding (curry, id, uncurry, (.), (<*>))

type family AsOp a = r | r -> a where
  AsOp Type.Unit = Void
  AsOp (a Type.* b) = AsOp a + AsOp b
  AsOp (a Type.~> b) = AsOp a -< AsOp b
  AsOp Type.U64 = U64

asMal :: Expr k a b -> k (AsOp b) (AsOp a)
asMal (E x) = x

newtype Expr k a b = E (k (AsOp b) (AsOp a))

instance Mal k => Lambda (Expr k)

instance Category k => Category (Expr k) where
  id = E id
  E f . E g = E (g . f)

instance HasSum k => HasProduct (Expr k) where
  unit = E absurd

  E f &&& E g = E (f ||| g)
  first = E left
  second = E right

instance HasCoexp k => HasExp (Expr k) where
  curry (E f) = E (throw f)
  uncurry (E f) = E (try f)

-- instance Mal k => Term (Expr k) where
--   tip = E first
--   const (E x) = E (x . second)

--   E x `be` E f = E (curry f <*> x)

--   curry (E f) = E (curry f)
--   E f <*> E x = E (f <*> x)

--   u64 x = E (u64 x . unit)
--   add = E (add . unit)
