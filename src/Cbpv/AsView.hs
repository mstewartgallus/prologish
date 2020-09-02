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

view :: Code a b -> String
view (Code v) = v

instance Category Stack where
  id = Stack "id"
  Stack f . Stack g = Stack (g ++ " ∘ " ++ f)

instance Category Code where
  id = Code "id"
  Code f . Code g = Code (g ++ " ∘ " ++ f)

instance Cbpv Stack Code where
  to (Stack f) (Stack x) = Stack ("(to " ++ f ++ " " ++ x ++ ")")
  return (Code f) = Stack ("(return " ++ f ++ ")")

  thunk (Stack f) = Code ("(thunk " ++ f ++ ")")
  force (Code f) = Stack ("(force " ++ f ++ ")")

  unit = Code "unit"
  Code f &&& Code x = Code ("⟨" ++ f ++ " , " ++ x ++ "⟩")
  first = Code "π₁"
  second = Code "π₂"

  absurd = Code "absurd"
  Code f ||| Code x = Code ("[" ++ f ++ " , " ++ x ++ "]")
  left = Code "i₁"
  right = Code "i₂"

  assocOut = Stack "out"
  assocIn = Stack "in"

  curry (Stack f) = Stack ("(λ " ++ f ++ ")")
  uncurry (Stack f) = Stack ("(! " ++ f ++ ")")

  u64 x = Code (show x)
  add = Code "add"
  addLazy = Stack "add"
