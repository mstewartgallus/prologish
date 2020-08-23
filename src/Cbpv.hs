{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv (Cbpv (..)) where

import Cbpv.Sort
import Control.Category
import Data.Word (Word64)
import Prelude hiding ((.), (<*>), id)

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

  unit :: dta x Unit
  (#) :: dta env a -> dta env b -> dta env (a * b)
  first :: dta (a * b) a
  second :: dta (a * b) b

  absurd :: dta Void x
  (!) :: dta a c -> dta b c -> dta (a + b) c
  left :: dta a (a + b)
  right :: dta b (a + b)

  lambda :: stk (F (env * a)) b -> stk (F env) (a ~> b)
  (<*>) :: stk (F env) (a ~> b) -> dta env a -> stk (F env) b

  u64 :: Word64 -> dta x U64

  -- fixme.. have optimized version...
  add :: dta Unit (U (U64 ~> F (U (U64 ~> F U64))))

infixl 9 #

infixl 9 !
