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
import Prelude hiding (curry, id, uncurry, (&&&), (.), (<*>))

asLambda :: Expr k a b -> k a b
asLambda (E x) = x

newtype Expr k (a :: T) (b :: T) = E {e :: k a b}

instance Lambda k => Fn (Expr k a) where
  E f &&& E g = E (f &&& g)
  first = \(E x) -> E (first . x)
  second = \(E x) -> E (second . x)
  unit = E unit

  -- curry f = \x -> E (curry (e (f x)))
  E f <*> E x = E (f <*> x)

  u64 x = E (u64 x . unit)
  add = E (add . unit)
