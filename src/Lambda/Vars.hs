module Lambda.Vars (Vars (..)) where

import Control.Category
import Id (Id)
import Lambda.Type

class Category k => Vars k where
  bindImplicitEnv :: Id -> ST a -> (k Unit a -> k Unit b) -> k env a -> k env b
