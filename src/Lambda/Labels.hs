module Lambda.Labels (Labels (..)) where

import Control.Category
import Id (Id)
import Lambda.Type

class Category k => Labels k where
  bindMapLabel :: Id -> ST b -> (k b Void -> k a Void) -> k a b
