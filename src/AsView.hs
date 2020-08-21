{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module AsView (View, view) where

import Control.Category
import Control.Monad.State
import Exp
import Hoas
import Lambda
import Product
import Sum
import Type
import Prelude hiding ((.), id)

newtype View (a :: T) (b :: T) = View {unView :: State Int String}

view :: View a b -> String
view (View v) = evalState v 0

instance Category View where
  id = View $ pure "id"
  View f . View g = View $ do
    g' <- g
    f' <- f
    pure (g' ++ "\n" ++ f')

instance Product View where
  unit = View $ pure "unit"
  View x `letBe` View f = View $ do
    x' <- x
    f' <- f
    pure (x' ++ " be " ++ f')

  View f # View x = View $ do
    f' <- f
    x' <- x
    pure ("(" ++ f' ++ " Δ " ++ x' ++ ")")
  first = View $ pure ".0"
  second = View $ pure ".1"

instance Sum View where
  absurd = View $ pure "absurd"

  View f ! View x = View $ do
    f' <- f
    x' <- x
    pure ("(" ++ f' ++ " + " ++ x' ++ ")")
  left = View $ pure "#l"
  right = View $ pure "#r"

instance Exp View where
  lambda (View f) = View $ do
    f' <- f
    pure ("λ " ++ f')
  eval = View $ pure "!"

instance Lambda View where
  u64 x = View $ pure (show x)
  add = View $ pure "add"

instance Hoas View where
  mapVar t f = View $ do
    n <- fresh
    let v = "v" ++ show n
    body <- unView (f (View $ pure v))
    pure (v ++ ": " ++ show t ++ ".\n" ++ body)
  mapLabel t f = View $ do
    n <- fresh
    let v = "l" ++ show n
    body <- unView (f (View $ pure v))
    pure (v ++ ": " ++ show t ++ ".\n" ++ body)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  return n
