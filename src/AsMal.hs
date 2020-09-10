{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module AsMal (Expr, asMal, AsObject) where

import Control.Category
import qualified Hoas.Type as Type
import Mal
import Mal.HasCoexp
import Mal.HasSum
import Mal.Type
import Term (Term)
import qualified Term
import Prelude hiding (curry, id, uncurry, (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.~> b) = AsObject a -< AsObject b
  AsObject Type.U64 = U64

type family AsList a = r | r -> a where
  AsList '[] = Void
  AsList (h ': t) = AsObject h + AsList t

asMal :: Expr k '[] b -> k (AsObject b) Void
asMal (E x) = x

data Expr k (a :: [Type.T]) (b :: Type.T) where
  E :: k (AsObject b) (AsList a) -> Expr k a b

instance Mal k => Term (Expr k) where
  tip = E left
  const (E x) = E (right . x)

  curry (E f) = E (throw f)
  E f <*> E x = E (f <*> x)

-- u64 x = E (absurd . u64 x)
-- add = E (absurd . add)
