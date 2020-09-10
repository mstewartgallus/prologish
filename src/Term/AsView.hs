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
  View f `try` View x = View ("try {" ++ f ++ "} catch {" ++ x ++ "}")

  u64 n (View y) = View (show n ++ " = " ++ y)
  add = View "add"

  tip = View "I"
  const (View x) = View ("(K " ++ x ++ ")")

  mal (View f) = View ("(mal " ++ f ++ ")")
  done = View "done"
