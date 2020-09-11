{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

newtype View (a :: T) = V String

view :: View a -> String
view (V v) = v

instance Bound View where
  be n (V x) t f = V (x ++ " be " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        V body = f (V v)

  mal n t f = V (v ++ " : " ++ show t ++ " ⊨\n" ++ body) where
        v = "v" ++ show n
        V body = f (V v)

  V f `try` V x = V ("(" ++ f ++ " " ++ x ++ ")")

  isUnit (V x) = V $ "(isUnit " ++ x ++ ")"
  V x `isBoth` (V f, V g) = V $ "<" ++ x ++ " | " ++ f ++ ", " ++ g ++ ">"
  isFirst (V x) = V $ "(π₁ " ++ x ++ ")"
  isSecond (V x) = V $ "(π₂ " ++ x ++ ")"

  isAbsurd = V "isAbsurd"
  V f ||| V g = V ("[" ++ f ++ "; " ++ g ++ "]")
  isLeft (V x) = V $ "(i₁ " ++ x ++ ")"
  isRight (V x) = V $ "(i₂ " ++ x ++ ")"

  isU64 (V x) n = V $ "(" ++ x ++ " = " ++ show n ++ ")"
  add = V "add"
