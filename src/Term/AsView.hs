{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Term.AsView (View, view) where

import Term
import Hoas.Type

newtype View (a :: T)  (env :: [T]) = View String

view :: View a env -> String
view (View v) = v

instance Term View where
  View x `be` View f = View ("(" ++ x ++ " be " ++ f ++ ")")
  View f `try` View x = View $ "(" ++ f ++ " try " ++ x ++ ")"

  u64 n = View (show n)
  add (View x) (View y) = View $ "(" ++ x ++ " + " ++ y ++ ")"

  tip = View "I"
  const (View x) = View ("(K " ++ x ++ ")")

  mal (View f) = View ("(mal " ++ f ++ ")")
  absurd = View "absurd"
