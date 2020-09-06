{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsLambda (Expr, asLambda) where

import Control.Category
import Data.Kind
import Data.Maybe
import Data.Typeable ((:~:) (..))
import Fn (Fn)
import qualified Fn
import Id (Id)
import Lambda
import Lambda.Exp
import Lambda.Product
import Lambda.Type
import qualified Term.Type as Type
import Prelude hiding (curry, id, uncurry, (&&&), (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.~> b) = AsObject a ~> AsObject b
  AsObject Type.U64 = U64

asLambda :: Expr k '[a] b -> k (AsObject a) (AsObject b)
asLambda (E x) = x

data Expr k (a :: [Type.T]) (b :: Type.T) where
  E :: k (AsObject a) (AsObject b) -> Expr k '[a] b

instance Lambda k => Fn (Expr k) where
  -- curry f = \x -> E (curry (e (f x)))
  E f <*> E x = E (f <*> x)

-- u64 x = E (u64 x . unit)
-- add = E (add . unit)
