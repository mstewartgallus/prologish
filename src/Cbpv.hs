{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv (Cbpv (..)) where

import Cbpv.Sort
import Control.Category
import Data.Word (Word64)
import Prelude hiding ((.), id)

class (Category stk, Category dta) => Cbpv stk dta | stk -> dta, dta -> stk where
  thunk ::
    stk (F x) y ->
    dta x (U y)
  force ::
    dta x (U y) ->
    stk (F x) y

  returns ::
    dta env a ->
    stk (F env) (F a)
  to ::
    stk (env & k) (F a) ->
    stk (F (env * a)) b ->
    stk (env & k) b

  -- fixme, do the empty stoup and tensor stuff...
  initial :: stk x I

  unit :: dta x Unit
  (#) :: dta env a -> dta env b -> dta env (a * b)
  first :: dta (a * b) a
  second :: dta (a * b) b

  absurd :: dta Void x
  (!) :: dta a c -> dta b c -> dta (a + b) c
  left :: dta a (a + b)
  right :: dta b (a + b)

  lambda :: dta (a * b) (U c) -> stk (F a) (b ~> c)
  eval :: stk (F a) (b ~> c) -> dta (a * b) (U c)

  u64 :: Word64 -> dta x U64

  -- fixme.. have optimized version...
  add :: stk (F Unit) (U (F U64) ~> U (F U64) ~> F U64)

infixl 9 #

infixl 9 !
