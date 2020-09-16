{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Mal.AsView (View, view) where

import Control.Category
import Mal
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type

newtype View (a :: T) (b :: T) = V String

view :: View a b -> String
view (V v) = v

instance Category View where
  id = V "id"
  V f . V g = V (f ++ " ∘ " ++ g)

instance HasProduct View where
  unit = V "unit"

  V f &&& V x = V ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = V "π₁"
  second = V "π₂"

instance HasSum View where
  absurd = V "absurd"

  V f ||| V x = V ("[" ++ f ++ " ; " ++ x ++ "]")
  left = V "i₁"
  right = V "i₂"

instance HasCoexp View where
  mal (V f) = V ("(mal " ++ f ++ ")")
  try (V f) = V ("(! " ++ f ++ ")")

instance Mal View where
  add = V "add"
  u64 x = V (show x)
