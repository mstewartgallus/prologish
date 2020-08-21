{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module LabelHoas (LabelHoas (..), label) where

import Control.Category
import Sum
import Type
import Prelude hiding ((.), id)

class Category k => LabelHoas k where
  mapLabel :: ST b -> (k b Void -> k a Void) -> k a b

label :: (LabelHoas k, Sum k) => ST env -> ST b -> (k b Void -> k a env) -> k a (env + b)
label env t f = mapLabel (env :+: t) $ \x -> x . left . f (x . right)
