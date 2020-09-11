{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

newtype View (a :: T) = View String

view :: View a -> String
view (View v) = v

instance Bound View where
  be n (View x) t f = View (x ++ " be " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        View body = f (View v)

  mal n t f = View ("mal " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        View body = f (View v)

  View f `try` View x = View ("try { " ++ f ++ " } catch { " ++ x ++ " }")

  isUnit (View x) = View $ "(isUnit " ++ x ++ ")"
  View x `isBoth` (View f, View g) = View (x ++ " = <" ++ f ++ ", " ++ g ++ ">")
  isFirst (View x) = View $ "(isFirst " ++ x ++ ")"
  isSecond (View x) = View $ "(isSecond " ++ x ++ ")"

  isAbsurd = View "isAbsurd"
  isEither (View f) (View g) = View ("[" ++ f ++ "; " ++ g ++ "]")
  isLeft (View x) = View $ "(isLeft " ++ x ++ ")"
  isRight (View x) = View $ "(isRight " ++ x ++ ")"

  isU64 (View x) n = View ("(" ++ x ++ " = " ++ show n ++ ")")
  add = View "add"
