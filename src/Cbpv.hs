{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv (Cbpv (..)) where

import Control.Category
import Data.Word (Word64)
import Sort
import Prelude hiding ((.), id)

class (Category cd, Category dta) => Cbpv cd dta | cd -> dta, dta -> cd where
  thunk :: cd (F x) b -> dta x (U b)
  force :: dta x (U y) -> cd (F x) y

  box :: dta x (U (F x))
  box = thunk id

  unbox :: cd (F (U x)) x
  unbox = force id

  initial :: cd x Initial

  unit :: dta x Unit
  (#) :: dta env a -> dta env b -> dta env (a * b)
  first :: dta (a * b) a
  second :: dta (a * b) b

  absurd :: cd Void x
  (!) :: dta a c -> dta b c -> dta (a + b) c
  left :: dta a (a + b)
  right :: dta b (a + b)

  u64 :: Word64 -> dta x U64
  add :: cd x (U64 ~> U64 ~> F U64)

infixl 9 #

infixl 9 !
