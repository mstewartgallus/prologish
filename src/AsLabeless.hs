{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsLabeless (Labeless, removeLabels) where

import Control.Category
import Data.Typeable ((:~:) (..))
import Exp
import Labels
import Lambda
import Product
import Sum
import Type
import Prelude hiding ((.), (<*>), id)

removeLabels :: Sum k => Labeless k a b -> k a b
removeLabels (L x) = (id ! absurd) . x EmptyCase

newtype Labeless k (a :: T) b = L (forall c. Case c -> k a (b + c))

data Case a where
  EmptyCase :: Case Void
  LabelCase :: Label l -> Case a -> Case (a + l)

instance Sum k => Category (Labeless k) where
  id = L $ const left
  L f . L g = L $ \env -> (f env ! right) . g env

instance (Sum k, Exp k) => Labels (Labeless k) where
  mkLabel v = L $ \env -> right . matchLabels v env
  bindLabel v (L x) = L $ \env ->
    let shuffle :: Sum k => k (a + (c + b)) ((a + b) + c)
        shuffle = undefined
     in shuffle . x (LabelCase v env)

instance (Sum k, Exp k) => Product (Labeless k) where
  unit = L $ const (left . unit)
  L f # L g = L $ \env -> distribute (f env) (g env)
  first = L $ const (left . first)
  second = L $ const (left . second)

instance Sum k => Sum (Labeless k) where
  absurd = L $ const (left . absurd)
  L f ! L g = L $ \env -> f env ! g env
  left = L $ const (left . left)
  right = L $ const (left . right)

instance (Sum k, Exp k) => Exp (Labeless k) where
  lambda = undefined
  eval = L $ const (left . eval)

instance Lambda k => Lambda (Labeless k) where
  u64 x = L $ const (left . u64 x)
  add = L $ const (left . add)

distribute :: (Sum k, Exp k) => k c (a + x) -> k c (b + x) -> k c ((a * b) + x)
distribute f g = unlambda (lambda (unlambda bar . (second # first)) ! lambda (right . first)) . (f # g)

bar :: (Sum k, Exp k) => k (b + x) (a ~> ((a * b) + x))
bar = lambda (left . (second # first)) ! lambda (right . first)

matchLabels :: Sum k => Label a -> Case c -> k a c
matchLabels x cases = case cases of
  LabelCase y rest -> case x `eqLabel` y of
    Just Refl -> right
    Nothing -> left . matchLabels x rest
  _ -> undefined
