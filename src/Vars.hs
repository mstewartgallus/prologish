{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Vars (Vars (..)) where

import Control.Category
import Data.Typeable ((:~:) (..))
import Id (Id)
import Type
import Prelude hiding ((.), id)

class Vars k where
  bindMapVar :: Id -> ST a -> (k Unit a -> k Unit b) -> k a b
