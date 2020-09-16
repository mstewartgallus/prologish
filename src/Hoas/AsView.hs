{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

data View (a :: T) (b :: T) = E String

view :: View a b -> String
view (E v) = v

instance Bound View where
  mal n t f = E ("mal " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        E body = f (E v)
  E f `try` E x = E ("(" ++ f ++ " " ++ x ++ ")")

  E f ||| E x = E $ "[" ++ f ++ " ; " ++ x ++ "]"

  var = E "id"

  E f `jump` E x = E ("⟨" ++ f ++ " | " ++ x ++ "⟩")

  unit = E "unit"
  empty = E "empty"

  u64 n = E (show n)
