{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

newtype View (a :: T) = V String

view :: View a -> String
view (V v) = v

instance Bound View where
  kont n t (V x) k = V (x ++ " kont " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        V body = k (V v)
  val (V f) = V $ "(val " ++ f ++ ")"
  jump (V f) (V x) = V $ "(" ++ f ++ " " ++ x ++ ")"

  unit = V "unit"
  V x &&& V y = V $ "<" ++ x ++ ", " ++ y ++ ">"
  first (V x) = V $ "(π₁ " ++ x ++ ")"
  second (V x) = V $ "(π₂ " ++ x ++ ")"

  absurd (V x) = V $ "(absurd " ++ x ++ ")"
  -- V x `isEither` (V f, V g) = V $ "[" ++ x ++ " | " ++ f ++ "; " ++ g ++ "]"

  left (V x) = V $ "(i₁ " ++ x ++ ")"
  right (V x) = V $ "(i₂ " ++ x ++ ")"

  pick (V x) = V $ "(pick " ++ x ++ ")"
  true = V "true"
  false = V "false"

  u64 n = V $ show n
  add (V x) (V y) = V $ "(" ++ x ++ " + " ++ y ++ ")"
