{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Term.AsView (View, view) where

import Term
import Hoas.Type

newtype View (env :: [T]) (a :: T) = V String

view :: View env a -> String
view (V v) = v

instance Term View where
  u64 n = V (show n)
  add (V x) (V y) = V $ "(" ++ x ++ " + " ++ y ++ ")"

  tip = V "I"
  const (V x) = V ("(K " ++ x ++ ")")

  unit = V "unit"

  absurd (V x) = V $ "(absurd " ++ x ++ ")"
  left (V x) = V $ "(left " ++ x ++ ")"
  right (V x) = V $ "(right " ++ x ++ ")"

  mal (V x) (V k) = V ("(mal " ++ x ++ " " ++ k ++ ")")
  try (V x) (V k) (V y) = V ("(try " ++ x ++ " " ++ k ++ " " ++ y ++ ")")

  swap (V x) = V $ "(swap " ++ x ++ ")"
