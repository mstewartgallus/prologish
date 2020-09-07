{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Term.AsView (View, view) where

import Term
import HasApply
import HasWord
import Hoas.Type

newtype View (env :: [T]) (a :: T) = View String

view :: View env a -> String
view (View v) = v

instance HasApply (View env) where
  View f <*> View x = View ("(" ++ f ++ " " ++ x ++ ")")

instance HasWord (View env) where
  u64 n = View (show n)

instance Term View where
  View x `be` View f = View ("(" ++ x ++ " be " ++ f ++ ")")

  add = View "add"

  tip = View "I"
  const (View x) = View ("(K " ++ x ++ ")")

  curry (View f) = View ("(λ " ++ f ++ ")")
