{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cbpv.AsView (Code, view) where

import Cbpv
import Control.Category
import Cbpv.Sort
import Prelude hiding ((.), id)

newtype Code (a :: Algebra) (b :: Algebra) = Code String

newtype Data (a :: Set) (b :: Set) = Data String

view :: Code a b -> String
view (Code v) = v

instance Category Code where
  id = Code "id"
  Code f . Code g = Code (g ++ " ∘ " ++ f)

instance Category Data where
  id = Data "id"
  Data f . Data g = Data (g ++ " ∘ " ++ f)

instance Cbpv Code Data where
  to (Code f) = Data ("(to " ++ f ++ ")")
  returns (Data f) = Code ("(returns " ++ f ++ ")")

  thunk (Code f) = Data ("(thunk " ++ f ++ ")")
  force (Data f) = Code ("(force " ++ f ++ ")")

  unit = Data "unit"
  Data f # Data x = Data ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = Data "π₁"
  second = Data "π₂"

  absurd = Data "absurd"
  Data f ! Data x = Data ("[" ++ f ++ " , " ++ x ++ "]")
  left = Data "i₁"
  right = Data "i₂"

  lambda (Data f) = Code ("(λ " ++ f ++ ")")
  eval (Code f) = Data ("(! " ++ f ++ ")")

  u64 x = Data (show x)
  add = Code "add"
