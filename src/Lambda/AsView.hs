{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.AsView (View, view) where

import Control.Category
import Lambda.HasExp
import Lambda
import Lambda.HasProduct
import Lambda.HasSum
import Lambda.Type

newtype View (a :: T) (b :: T) = View String

view :: View a b -> String
view (View v) = v

instance Category View where
  id = View "id"
  View f . View g = View (f ++ " ∘ " ++ g)

instance HasProduct View where
  unit = View "unit"

  View f &&& View x = View ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = View "π₁"
  second = View "π₂"

instance HasSum View where
  absurd = View "absurd"

  View f ||| View x = View ("[" ++ f ++ " , " ++ x ++ "]")
  left = View "i₁"
  right = View "i₂"

instance HasExp View where
  curry (View f) = View ("(λ " ++ f ++ ")")
  uncurry (View f) = View ("(! " ++ f ++ ")")

instance Lambda View where
  u64 x = View (show x)
  add = View "add"
