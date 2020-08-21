{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Labels (Labels (..), Label (..), eqLabel) where

import Data.Typeable ((:~:) (..))
import Sum
import Type
import Prelude hiding ((.), id)

class Sum k => Labels k where
  mkLabel :: Label a -> k a x
  bindLabel :: Label b -> k a Void -> k a b

data Label a = Label (ST a) Int

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing
