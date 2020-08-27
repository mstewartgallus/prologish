module Lambda.Labels (Labels (..)) where

import Control.Category
import Id (Id)
import Lambda.Type

class Category k => Labels k where
  bindImplicitLabel :: Id -> ST b -> (k b Void -> k a Void) -> k b env -> k a env
