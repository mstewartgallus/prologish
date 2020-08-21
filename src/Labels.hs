{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Labels (Labels (..), Label (..), eqLabel) where

import Control.Category
import Data.Typeable ((:~:) (..))
import Id (Id)
import Type
import Prelude hiding ((.), id)

class Category k => Labels k where
  mkLabel :: Label a -> k a x
  bindLabel :: Label b -> k a Void -> k a b

data Label a = Label (ST a) Id

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing
