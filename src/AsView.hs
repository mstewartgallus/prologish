{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module AsView (View, view) where

import Control.Category
import Global
import HasCoexp
import HasProduct
import HasSum
import Hoas.Bound
import Mal
import Type

data View (a :: T) (b :: T) = V String

view :: View a b -> String
view (V v) = v

instance Category View where
  id = V "id"
  V f . V g = V (f ++ " ∘ " ++ g)

instance Bound View where
  st n t f = V ("{ " ++ v ++ ": " ++ show t ++ " |\n" ++ body ++ "}")
    where
      v = "s" ++ show n
      V body = f (V v)
  try (V f) (V x) = V $ "(" ++ x ++ " ∈ " ++ f ++ ")"

  V f ||| V x = V $ "(" ++ f ++ " ∪ " ++ x ++ ")"

  u64 n = V (show n)
  global g = V (show g)

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
  st (V f) = V ("{|" ++ f ++ "}")
  try (V f) = V ("(∋ " ++ f ++ ")")

instance Mal View where
  global g = V (show g)

  u64 x = V (show x)
