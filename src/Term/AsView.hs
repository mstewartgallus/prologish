{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Term.AsView (View, view) where

import Term
import Hoas.Type

newtype View (a :: T)  (env :: [T]) = V String

view :: View a env -> String
view (V v) = v

instance Term View where
  V f `try` V x = V $ "(" ++ f ++ " try " ++ x ++ ")"

  V x `isU64` n = V ("(" ++ x ++ " = " ++ show n ++ ")")
  add = V "add"

  tip = V "I"
  const (V x) = V ("(K " ++ x ++ ")")

  mal (V f) = V ("(⊦ " ++ f ++ ")")

  V x `isBoth` (V f, V g) = V $ "<" ++ x ++ " | " ++ f ++ ", " ++ g ++ ">"

  isLeft (V x) = V $ "(isLeft " ++ x ++ ")"

  absurd = V "absurd"
  V f ||| V g = V $ "[" ++ f ++ "; " ++ g ++ "]"
