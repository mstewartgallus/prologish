{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.AsView (View, view) where

import Control.Category
import Lambda.Exp
import Lambda
import Lambda.Bound
import Lambda.Product
import Lambda.Sum
import Lambda.Type

newtype View (a :: T) (b :: T) = View String

view :: View a b -> String
view (View v) = v

instance Bound (View a) where
  lam n t f = View ("\\" ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        View body = f (View v)

  View f <*> View x = View ("(" ++ f ++ " " ++ x ++ ")")

  u64 n = View (show n)
  add = View "add"

instance Category View where
  id = View "id"
  View f . View g = View (g ++ " ∘ " ++ f)

instance Product View where
  unit = View "unit"

  View f &&& View x = View ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = View "π₁"
  second = View "π₂"

instance Sum View where
  absurd = View "absurd"

  View f ||| View x = View ("[" ++ f ++ " , " ++ x ++ "]")
  left = View "i₁"
  right = View "i₂"

instance Exp View where
  curry (View f) = View ("(λ " ++ f ++ ")")
  uncurry (View f) = View ("(! " ++ f ++ ")")

instance Lambda View where
  u64 x = View (show x)
  add = View "add"
