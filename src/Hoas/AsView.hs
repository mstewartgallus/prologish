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

  u64 n (View y) = View (show n ++ " = " ++ y)
  add = View "add"

  done = View "done"
