{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module HasDomain (HasDomain (..)) where

import Control.Category
import Data.Proxy
import Type
import Prelude hiding (curry, id, uncurry, (.), (<*>))

class Category k => HasDomain k where
  domain :: Proxy (k a b) -> ST a
  range :: Proxy (k a b) -> ST b
