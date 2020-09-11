{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

newtype View (a :: T) = V String

view :: View a -> String
view (V v) = v

instance Bound View where
  mal n t (V x) k = V (x ++ " ⊨ " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        V body = k (V v)
  assume (V x) = V $ "(assume " ++ x ++ ")"
  deny (V x) (V y) = V $ "(deny " ++ x ++ " " ++ y ++ ")"

  unit = V "unit"
  V x &&& V y = V $ "<" ++ x ++ ", " ++ y ++ ">"
  first (V x) = V $ "(π₁ " ++ x ++ ")"
  second (V x) = V $ "(π₂ " ++ x ++ ")"

  absurd (V x) = V $ "(absurd " ++ x ++ ")"
  V x `isEither` (V f, V g) = V $ "[" ++ x ++ " | " ++ f ++ "; " ++ g ++ "]"

  left (V x) = V $ "(i₁ " ++ x ++ ")"
  right (V x) = V $ "(i₂ " ++ x ++ ")"

  pick (V x) = V $ "(pick " ++ x ++ ")"
  true = V "true"
  false = V "false"

  isU64 (V x) n = V $ "(" ++ x ++ " = " ++ show n ++ ")"
