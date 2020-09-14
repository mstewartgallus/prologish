{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type

newtype View (a :: T) = V String

view :: View a -> String
view (V v) = v

instance Bound View View where
  kont n t (V x) k = V (x ++ " \\ " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        V body = k (V v)
  jump n t (V f) k = V (f ++ " ! " ++ v ++ " : " ++ show t ++ ".\n" ++ body) where
        v = "v" ++ show n
        V body = k (V v)
  val = op1 "val"

  unit = V "unit"
  V x &&& V y = V $ "<" ++ x ++ ", " ++ y ++ ">"
  first = op1 "π₁"
  second = op1 "π₂"

  absurd = V "absurd"
  V x ||| V y = V $ "[" ++ x ++ " ; " ++ y ++ " ]"
  left = op1 "i₁"
  right = op1 "i₂"

  pick = op1 "pick"
  true = V "true"
  false = V "false"

  u64 n = V $ show n
  add (V x) (V y) = V $ "(" ++ x ++ " + " ++ y ++ ")"

op1 :: String -> View a -> View b
op1 name (V x) = V $ "(" ++ name ++ " " ++ x ++ ")"
