{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module View (View) where

import Control.Category
import Lam
import Type

newtype View (a :: T) (b :: T) = View String

instance Category View where
  id = View "id"
  View f . View g = View $ g ++ "\n" ++ f

instance Lam View where
  View x `letBe` View f = View $ x ++ " be " ++ f

  View f # View x = View $ f ++ " Δ " ++ x
  first = View ".0"
  second = View ".1"

  View f ! View x = View $ f ++ " + " ++ x
  left = View "#l"
  right = View "#r"

  lambda (View f) = View $ "λ " ++ f
  eval = View "!"

  u64 x = View (show x)
  add = View "+"

instance Show (View a b) where
  show (View x) = x
