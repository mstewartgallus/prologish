{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module AsView (View, view) where

import Control.Category
import Exp
import Labels
import Lambda
import Product
import Sum
import Type
import Vars
import Prelude hiding ((.), id)

newtype View (a :: T) (b :: T) = View String

view :: View a b -> String
view (View v) = v

instance Category View where
  id = View "id"
  View f . View g = View (g ++ " ∘ " ++ f)

instance Product View where
  unit = View "unit"
  View x `letBe` View f = View (x ++ " be " ++ f)

  View f # View x = View ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = View "π₁"
  second = View "π₂"

instance Sum View where
  absurd = View "absurd"

  View f ! View x = View ("[" ++ f ++ " , " ++ x ++ "]")
  left = View "i₁"
  right = View "i₂"

instance Exp View where
  lambda (View f) = View ("(λ " ++ f ++ ")")
  eval (View f) = View ("(! " ++ f ++ ")")

instance Lambda View where
  u64 x = View (show x)
  add = View "add"

instance Vars View where
  bindMapVar n t f =
    let v = "v" ++ show n
        View body = f (View v)
     in View (v ++ ": " ++ show t ++ ".\n" ++ body)

instance Labels View where
  bindMapLabel n t f =
    let v = "l" ++ show n
        View body = f (View v)
     in View (v ++ ": " ++ show t ++ ".\n" ++ body)
