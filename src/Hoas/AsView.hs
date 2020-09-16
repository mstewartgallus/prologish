{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type
import Control.Category

data View (a :: T) (b :: T) = E String

view :: View a b -> String
view (E v) = v

instance Category View where
  id = E "id"
  E f . E x = E ("⟨" ++ f ++ " | " ++ x ++ "⟩")

instance Bound View where
  letLabel n t f = E (v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        E body = f (E v)

  mal (E f) = E $ "(mal " ++ f ++ ")"
  E f `try` E x = E ("(" ++ f ++ " " ++ x ++ ")")

  E f ||| E x = E $ "[" ++ f ++ " ; " ++ x ++ "]"

  unit = E "unit"
  empty = E "empty"

  u64 n = E (show n)
