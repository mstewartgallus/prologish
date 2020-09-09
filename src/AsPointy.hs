{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsPointy (AsPointy, asPointy) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import qualified Hoas
import qualified Hoas.Type as Type
import Id (Id)
import Lambda.Type
import Pointy (Pointy)
import qualified Pointy
import Prelude hiding (curry, id, uncurry, (.), (<*>))

asPointy :: AsPointy t a -> t Unit -> t (AsObject a)
asPointy (AsPointy x) = x

type family AsObject a = r | r -> a where
  AsObject (a Type.~> b) = AsObject a ~> AsObject b
  AsObject Type.U64 = U64

newtype AsPointy k b = AsPointy (k Unit -> k (AsObject b))

instance Pointy k => Hoas.Hoas (AsPointy k) where
  AsPointy f <*> AsPointy x = AsPointy (f Pointy.<*> x)

  lam t f = AsPointy $ \x ->
    ( Pointy.curry $ \y ->
        let AsPointy body = f (AsPointy $ \_ -> Pointy.first y)
         in body (Pointy.second y)
    )
      x

  u64 x = AsPointy (Pointy.u64 x)
  add = AsPointy (Pointy.add)
