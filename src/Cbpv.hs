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
  to :: cd env (F a) -> dta (U env) a
  returns :: dta (U env) a -> cd env (F a)

  thunk :: cd (F x) y -> dta x (U y)
  force :: dta x (U y) -> cd (F x) y

  unit :: dta x Unit
  (#) :: dta env a -> dta env b -> dta env (a * b)
  first :: dta (a * b) a
  second :: dta (a * b) b

  absurd :: dta Void x
  (!) :: dta a c -> dta b c -> dta (a + b) c
  left :: dta a (a + b)
  right :: dta b (a + b)

  lambda :: dta (a * b) (U c) -> cd (F a) (b ~> c)
  eval :: cd (F a) (b ~> c) -> dta (a * b) (U c)

  u64 :: Word64 -> dta x U64
  add :: cd x (U64 ~> U64 ~> F U64)

infixl 9 #

infixl 9 !
