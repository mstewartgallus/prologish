{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Mal.AsView (View, view) where

import Control.Category
import Mal.HasCoexp
import Mal
import Mal.HasSum
import Mal.HasProduct
import Mal.Type

newtype View (a :: T) (b :: T) = View String

view :: View a b -> String
view (View v) = v

instance Category View where
  id = View "id"
  View f . View g = View (f ++ " ∘ " ++ g)

instance HasProduct View where
  unit = View "unit"

  View f &&& View x = View ("<" ++ f ++ ", " ++ x ++ ">")
  first = View "π₁"
  second = View "π₂"

instance HasSum View where
  absurd = View "absurd"

  View f ||| View x = View ("[" ++ f ++ "; " ++ x ++ "]")
  left = View "i₁"
  right = View "i₂"

instance HasCoexp View where
  mal (View f) = View ("(⊦ " ++ f ++ ")")
  try (View f) = View ("(try " ++ f ++ ")")

instance Mal View where
  u64 x = View (show x)
--   add = View "add"
