{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

data View

view :: Expr View a -> String
view (E v) = v

instance Bound View where
  newtype Jump View = J String
  newtype Expr View (a :: T) = E String
  newtype Case View (a :: T) = C String

  adbmal m s n t f = C ("abdmal " ++ k ++ ": " ++ show s ++ ", " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        k = "k" ++ show m
        v = "v" ++ show n
        J body = f (C k) (E v)
  C f `try` C x = C ("(" ++ f ++ " " ++ x ++ ")")

  C f ||| C x = C $ "[" ++ f ++ " ; " ++ x ++ "]"

  thunk n t f = E ("thunk " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        J body = f (C v)
  letBe n t f = C ("let " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        J body = f (E v)


  C f `jump` E x = J ("⟨" ++ f ++ " | " ++ x ++ "⟩")

  unit = E "unit"
  empty = C "empty"

  u64 n = E (show n)
