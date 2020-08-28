{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.Optimize (optimize) where

import Control.Category
import Control.Monad.State
import Lambda.Exp
import Lambda.Hoas
import Id (Stream (..))
import Lambda.Labels
import Lambda
import Lambda.Product
import Lambda.Sum
import Lambda.Type
import Lambda.Vars
import Lambda.AsConcrete
import Prelude hiding ((.), id, (&&&), (|||), curry, uncurry, Either (..))
import Data.Word

optimize :: Expr k a b -> Expr k a b
optimize = w 50 where
  w n expr | n > 0 = w (n - 1) (opt expr)
           | otherwise = expr

opt :: Expr k a b -> Expr k a b
opt expr = case expr of
  Id :.: f -> opt f
  f :.: Id -> opt f

  f :.: g -> opt f :.: opt g

  Fanout f g -> Fanout (opt f) (opt g)
  Fanin f g -> Fanin (opt f) (opt g)

  Curry f -> Curry (opt f)
  Uncurry f -> Uncurry (opt f)

  _ -> expr
