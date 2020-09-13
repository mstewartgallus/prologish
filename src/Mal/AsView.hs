{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Mal.AsView (View, view) where

import Control.Category
import Mal.HasCoexp
import Mal
import Mal.HasSum
import Mal.HasProduct
import Mal.Type

newtype View (a :: T) (b :: T) = V String

view :: View a b -> String
view (V v) = v

instance Category View where
  id = V "id"
  V f . V g = V (f ++ " ∘ " ++ g)

instance HasProduct View where
  unit = V "unit"

  V f &&& V x = V ("<" ++ f ++ ", " ++ x ++ ">")
  first = V "π₁"
  second = V "π₂"

instance HasSum View where
  absurd = V "absurd"

  V f ||| V g = V ("[" ++ f ++ "; " ++ g ++ "]")
  left = V "i₁"
  right = V "i₂"

instance HasCoexp View where
  mal (V f) = V ("(⊨ " ++ f ++ ")")
  try (V f) = V ("(try " ++ f ++ ")")

instance Mal View where
  pick = V "pick"
  u64 x = V (show x)
  add (V x) (V y) = V $ "(" ++ x ++ " + " ++ y ++ ")"
