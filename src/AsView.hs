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

instance Cokappa View where
  label n t f = V ("κ " ++ v ++ ": " ++ show t ++ ".\n" ++ body)
    where
      v = "k" ++ show n
      V body = f (V v)
  lift (V x) = V $ "(lift " ++ x ++ ")"

instance Cozeta View where
  mal n t f = V ("ζ " ++ v ++ ": " ++ show t ++ ".\n" ++ body)
    where
      v = "k" ++ show n
      V body = f (V v)
  pass (V x) = V $ "(pass " ++ x ++ ")"

instance Bound View where
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
  mal (V f) = V ("(mal " ++ f ++ ")")
  try (V f) = V ("(! " ++ f ++ ")")

instance Mal View where
  global g = V (show g)

  u64 x = V (show x)
