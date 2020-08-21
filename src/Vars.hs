module Vars (Vars (..)) where

import Control.Category
import Id (Id)
import Type

class Category k => Vars k where
  bindMapVar :: Id -> ST a -> (k Unit a -> k Unit b) -> k a b
