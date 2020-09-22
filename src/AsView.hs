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

  -- not sure which direction is best ⊊ or ⊋
  V f . V g = V (f ++ " ⊊ " ++ g)

instance Bound View where
  st n t f = V ("{" ++ v ++ ": " ++ show t ++ " | ⊥ ← " ++ body ++ "}")
    where
      v = "x" ++ show n
      V body = f (V v)

  V f `try` V x = V $ "(" ++ x ++ " \\ " ++ f ++ ")"

  V x `amb` V y = V $ "(" ++ x ++ " amb " ++ y ++ ")"

  true = V "true"
  false = V "false"

  u64 n = V $ "{" ++ show n ++ "}"
  global g = V (show g)

instance HasProduct View where
  unit = V "unit"

  V f &&& V x = V ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = V "π₁"
  second = V "π₂"

instance HasSum View where
  absurd = V "∅"

  V f ||| V x = V ("[" ++ f ++ " ; " ++ x ++ "]")
  left = V "i₁"
  right = V "i₂"

instance HasCoexp View where
  st (V f) = V ("(← " ++ f ++ ")")
  try (V f) = V ("(\\ " ++ f ++ ")")

instance Mal View where
  V x `amb` V y = V $ "(" ++ x ++ " amb " ++ y ++ ")"

  global g = V (show g)

  true = V "true"
  false = V "false"

  u64 x = V $ "{" ++ show x ++ "}"
