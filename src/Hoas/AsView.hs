{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

newtype View (a :: T) = V String

view :: View a -> String
view (V v) = v

instance Bound View where
  kont n t (V x) k = V (x ++ " ⊨ " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        V body = k (V v)
  jump n t (V f) k = V (f ++ " ! " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        V body = k (V v)
  val (V x) = V $ "(val " ++ x ++ ")"

  unit = V "unit"
  V x &&& V y = V $ "<" ++ x ++ ", " ++ y ++ ">"
  first (V x) = V $ "(π₁ " ++ x ++ ")"
  second (V x) = V $ "(π₂ " ++ x ++ ")"

  absurd (V x) = V $ "(absurd " ++ x ++ ")"
  left (V x) = V $ "(i₁ " ++ x ++ ")"
  right (V x) = V $ "(i₂ " ++ x ++ ")"

  pick (V x) = V $ "(pick " ++ x ++ ")"
  true = V "true"
  false = V "false"

  u64 n = V $ show n
  add (V x) (V y) = V $ "(" ++ x ++ " + " ++ y ++ ")"
