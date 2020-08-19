{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Lam
import Type

class Lam k => Hoas k where
  var :: ST a -> (Object k a -> k env b) -> k (env * a) b
  label :: ST a -> (Point k a -> k b env) -> k b (env + a)

  fn :: ST a -> (Object k a -> k env b) -> k env (a ~> b)
  fn t f = lambda (var t f)
