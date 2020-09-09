{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsPointy (PointFree, pointFree) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import qualified Hoas.Bound as Bound
import qualified Hoas.Type as Type
import Id (Id)
import Lambda.Type
import Pointy (Pointy)
import qualified Pointy
import Prelude hiding (curry, id, uncurry, (.), (<*>))

pointFree :: PointFree t a -> t Unit -> t (AsObject a)
pointFree (PointFree x) = x

type family AsObject a = r | r -> a where
  AsObject (a Type.~> b) = AsObject a ~> AsObject b
  AsObject Type.U64 = U64

newtype PointFree k b = PointFree (k Unit -> k (AsObject b))

instance Pointy k => Bound.Bound (PointFree k) where
  PointFree f <*> PointFree x = PointFree (f Pointy.<*> x)

  lam n t f = PointFree $ \x ->
    ( Pointy.curry $ \y ->
        let PointFree body = f (PointFree $ \_ -> Pointy.first y)
         in body (Pointy.second y)
    )
      x

  u64 x = PointFree (Pointy.u64 x)
  add = PointFree (Pointy.add)
