{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Term.AsView (View, view) where

import Term.Bound
import Term.Type

newtype View (a :: T) = View String

view :: View a -> String
view (View v) = v

instance Bound View where
  lam n t f = View ("λ " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        View body = f (View v)

  View f <*> View x = View ("(" ++ f ++ " " ++ x ++ ")")

  u64 n = View (show n)
  add = View "add"
