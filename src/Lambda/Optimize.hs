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
opt = inline . dead . simp . canon

simp :: Expr k a b -> Expr k a b
simp expr = case expr of
  Id :.: f -> f
  f :.: Id -> f

  Fanout First Second -> Id
  Fanin Left Right -> Id

  Fanout Coin x :.: f -> simp $ Fanout Coin (x :.: f)
  Fanout x Coin :.: f -> simp $ Fanout (x :.: f) Coin

  Fanout (a :.: First) (b :.: Second) :.: Fanout x y -> simp $ Fanout (a :.: x) (b :.: y)
  Fanin x y :.: Fanin (Left :.: a) (Right :.: b) -> simp $ Fanin (x :.: a) (y :.: b)

  Curry (Uncurry f) -> simp f
  Uncurry (Curry f) -> simp f

  f :.: g -> simp f :.: simp g

  Fanout f g -> Fanout (simp f) (simp g)
  Fanin f g -> Fanin (simp f) (simp g)

  Curry f -> Curry (simp f)
  Uncurry f -> Uncurry (simp f)

  _ -> expr

cost :: Expr k a b -> Int
cost expr = case expr of
  f :.: g -> cost f + cost g

  Fanout f g -> 1 + cost f + cost g
  Fanin f g -> 1 + cost f + cost g

  Curry f -> 1 + cost f
  Uncurry f -> 1 + cost f

  _ -> 1

inline :: Expr k a b -> Expr k a b
inline expr = case expr of
  Fanout x y :.: f | cost f <= 20 -> inline (Fanout (x :.: f) (y :.: f))
  f :.: Fanin x y | cost f <= 20 -> inline (Fanin (f :.: x) (f :.: y))

  f :.: g -> inline f :.: inline g

  Fanout f g -> Fanout (inline f) (inline g)
  Fanin f g -> Fanin (inline f) (inline g)

  Curry f -> Curry (inline f)
  Uncurry f -> Uncurry (inline f)

  _ -> expr

canon :: Expr k a b -> Expr k a b
canon expr = case expr of
  (f :.: g) :.: h -> canon (f :.: (g :.: h))
  Fanout (Fanin x y) (Fanin a b) -> canon (Fanin (Fanout x a) (Fanout y b))

  f :.: g -> canon f :.: canon g

  Fanout f g -> Fanout (canon f) (canon g)
  Fanin f g -> Fanin (canon f) (canon g)

  Curry f -> Curry (canon f)
  Uncurry f -> Uncurry (canon f)

  _ -> expr

dead :: Expr k a b -> Expr k a b
dead expr = case expr of
  _ :.: Absurd -> Absurd
  Coin :.: _ -> Coin

  First :.: Fanout x _ -> dead x
  Second :.: Fanout _ x -> dead x

  Fanin x _ :.: Left -> dead x
  Fanin _ x :.: Right -> dead x

  Fanout _ Absurd -> Fanout Absurd Absurd
  Fanout Absurd _ -> Fanout Absurd Absurd

  Fanin _ Coin -> Fanin Coin Coin
  Fanin Coin _ -> Fanin Coin Coin

  f :.: g -> dead f :.: dead g

  Fanout f g -> Fanout (dead f) (dead g)
  Fanin f g -> Fanin (dead f) (dead g)

  Curry f -> Curry (dead f)
  Uncurry f -> Uncurry (dead f)

  _ -> expr
