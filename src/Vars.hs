{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Vars (Vars (..), Var (..), eqVar) where

import Control.Category
import Data.Typeable ((:~:) (..))
import Id (Id)
import Type
import Prelude hiding ((.), id)

class Vars k where
  mkVar :: Var a -> k x a
  bindVar :: Var a -> k Unit b -> k a b
  bindMapVar :: Id -> ST a -> (k Unit a -> k Unit b) -> k a b
  bindMapVar n t f =
    let v = Var t n
     in bindVar v (f (mkVar v))

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing
