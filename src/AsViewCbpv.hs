{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AsViewCbpv (Code, view) where

import Cbpv
import Control.Category
import Sort
import Prelude hiding ((.), id)

newtype Code (a :: Algebra) (b :: Algebra) = Code String

newtype Data (a :: Set) (b :: Set) = Data String

view :: Code a b -> String
view (Code v) = v

instance Category Code where
  id = Code "id"
  Code f . Code g = Code (g ++ "\n" ++ f)

instance Category Data where
  id = Data "id"
  Data f . Data g = Data (g ++ "\n" ++ f)

instance Cbpv Code Data where
  to (Code f) = Data ("(to " ++ f ++ ")")
  returns (Data f) = Code ("returns " ++ f ++ ")")

  thunk (Code f) = Data ("(thunk " ++ f ++ ")")
  force (Data f) = Code ("(force " ++ f ++ ")")

  unit = Data "unit"
  Data f # Data x = Data ("(" ++ f ++ " Δ " ++ x ++ ")")
  first = Data ".0"
  second = Data ".1"

  absurd = Data "absurd"
  Data f ! Data x = Data ("(" ++ f ++ " + " ++ x ++ ")")
  left = Data "#l"
  right = Data "#r"

  lambda (Data f) = Code ("(λ " ++ f ++ ")")
  eval (Code f) = Data ("(! " ++ f ++ ")")

  u64 x = Data (show x)
  add = Code "add"
