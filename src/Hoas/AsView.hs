{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsView (View, view) where

import Hoas.Bound
import Hoas.Type
import Control.Category
import Hoas.Global

data View (a :: T) (b :: T) = E String

view :: View a b -> String
view (E v) = v

instance Category View where
  id = E "id"
  E f . E x = E (f ++ " ∘ " ++ x)

instance Bound View where
  label n t f = E ("label " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        E body = f (E v)
  mal n t f = E ("mal " ++ v ++ ": " ++ show t ++ ".\n" ++ body) where
        v = "k" ++ show n
        E body = f (E v)

  pass (E x) = E $ "(pass " ++ x ++ ")"
  lift (E x) = E $ "(lift " ++ x ++ ")"

  u64 n = E (show n)

  global g = E (show g)
