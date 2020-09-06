{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type
import qualified HasWord
import qualified HasApply

newtype View (a :: T) = View String

view :: View a -> String
view (View v) = v

instance HasApply.HasApply View where
  View f <*> View x = View ("(" ++ f ++ " " ++ x ++ ")")

instance HasWord.HasWord View where
  u64 n = View (show n)

instance Bound View where
  be n (View x) t f = View (x ++ " be " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        View body = f (View v)

  lam n t f = View ("λ " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        View body = f (View v)

  add = View "add"
