{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cbpv.AsView (Stack, view) where

import Cbpv
import Control.Category
import Cbpv.Sort
import Prelude hiding ((.), id)

newtype Stack (a :: Algebra) (b :: Algebra) = Stack String

newtype Code (a :: Set) (b :: Set) = Code String

view :: Stack a b -> String
view (Stack v) = v

instance Category Stack where
  id = Stack "id"
  Stack f . Stack g = Stack (g ++ " ∘ " ++ f)

instance Category Code where
  id = Code "id"
  Code f . Code g = Code (g ++ " ∘ " ++ f)

instance Cbpv Stack Code where
  to (Stack f) (Stack x) = Stack ("(to " ++ f ++ " " ++ x ++ ")")
  returns (Code f) = Stack ("(returns " ++ f ++ ")")

  thunk (Stack f) = Code ("(thunk " ++ f ++ ")")
  force (Code f) = Stack ("(force " ++ f ++ ")")

  unit = Code "unit"
  Code f # Code x = Code ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = Code "π₁"
  second = Code "π₂"

  absurd = Code "absurd"
  Code f ! Code x = Code ("[" ++ f ++ " , " ++ x ++ "]")
  left = Code "i₁"
  right = Code "i₂"

  lambda (Code f) = Stack ("(λ " ++ f ++ ")")
  eval (Stack f) = Code ("(! " ++ f ++ ")")

  u64 x = Code (show x)
  add = Stack "add"
