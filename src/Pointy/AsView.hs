{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Pointy.AsView (View, view) where

import Pointy
import Control.Monad.State
import Lambda.Type

view :: (View a -> View b) -> String
view f = flip evalState 0 $ do
  x <- fresh
  v $ f (View $ pure x)

data View (a :: T) = View { v :: State Int String }

lam :: (View a -> View b) -> State Int String
lam f = do
  x <- fresh
  y' <- v $ f (View $ pure x)
  pure $ "(arr " ++ x ++ ". " ++ y' ++ ")"

toV :: State Int String -> View a -> View b
toV str (View env) = View $ do
  str' <- str
  env' <- env
  pure $ "[" ++ env' ++ "/]" ++ str'

subst :: (String -> State Int String) -> View a -> View b
subst f (View env) = View $ do
  x <- fresh
  y <- f x
  env' <- env
  pure $ "[" ++ env' ++ "/" ++ x ++ "]" ++ y

instance Pointy View where
  u64 n = \(View x) -> View $ do
    x' <- x
    pure $ "(" ++ show n ++ " " ++ x' ++ ")"
  add = \(View x) -> View $ do
    x' <- x
    pure $ "(add " ++ x' ++ ")"

  curry f = \(View env) -> View $ do
    k <- fresh
    y <- v $ f (View $ pure k)
    env' <- env
    pure $ "(λ " ++ k ++ ". " ++ y ++ ") " ++ env'
  uncurry f = \(View env) -> View $ do
    k <- fresh
    y <- v $ f (View $ pure k)
    env' <- env
    pure $ "(uncurry " ++ k ++ ". " ++ y ++ ") " ++ env'

  unit =  \(View x) -> View $ do
    x' <- x
    pure $ "unit " ++ x'
  f &&& g =  \(View env) -> View $ do
    k <- fresh
    f' <- v $ f (View $ pure k)
    g' <- v $ g (View $ pure k)
    env' <- env
    pure $ "[" ++ env' ++ "/" ++ k ++ "]<" ++ f' ++ ", " ++ g' ++ ">"

  first = \(View x) -> View $ do
    x' <- x
    pure $ "(π₁ " ++ x' ++ ")"
  second = \(View x) -> View $ do
    x' <- x
    pure $ "(π₂ " ++ x' ++ ")"

  absurd = \(View x) -> View $ do
    x' <- x
    pure $ "(absurd " ++ x' ++ ")"
  f ||| g = toV $ do
    f' <- lam f
    g' <- lam g
    pure $ "[" ++ f' ++ "; " ++ g' ++ "]"
  left = toV $ pure "left"
  right = toV $ pure "right"

fresh :: State Int String
fresh = do
  n <- get
  put (n + 1)
  return ("v" ++ show n)
