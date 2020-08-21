{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Labels (Labels (..)) where

import Control.Category
import Data.Typeable ((:~:) (..))
import Id (Id)
import Type
import Prelude hiding ((.), id)

class Category k => Labels k where
  bindMapLabel :: Id -> ST b -> (k b Void -> k a Void) -> k a b
