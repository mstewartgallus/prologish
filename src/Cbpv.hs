{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv (Cbpv (..)) where

import Cbpv.Sort
import Control.Category
import Data.Word (Word64)
import Prelude hiding (curry, id, return, uncurry, (.), (<*>))

-- |
-- As opposed to the usual monadic interface call by push value is based
-- around adjoint functors on two different categories.
--
-- There is a different formulation using oblique morphisms and an
-- indexed category instead of using the asymmetric tensor but was
-- difficult to work with.
--
-- Paul Blain Levy. "Call-by-Push-Value: A Subsuming Paradigm".
class (Category stk, Category dta) => Cbpv stk dta | stk -> dta, dta -> stk where
  thunk ::
    stk (F x) y ->
    dta x (U y)
  force ::
    dta x (U y) ->
    stk (F x) y

  return ::
    dta env a ->
    stk (F env) (F a)
  to ::
    stk (env & k) (F a) ->
    stk (F (env * a)) b ->
    stk (env & k) b

  unit :: dta x Unit
  (&&&) :: dta env a -> dta env b -> dta env (a * b)
  first :: dta (a * b) a
  second :: dta (a * b) b

  absurd :: dta Void x
  (|||) :: dta a c -> dta b c -> dta (a + b) c
  left :: dta a (a + b)
  right :: dta b (a + b)

  assocOut :: stk (a & (b & c)) ((a * b) & c)
  assocIn :: stk ((a * b) & c) (a & (b & c))

  curry :: stk (a & env) b -> stk env (a ~> b)
  uncurry :: stk env (a ~> b) -> stk (a & env) b

  u64 :: Word64 -> dta x U64

  -- fixme.. have optimized version...
  add :: dta Unit (U (U64 ~> F (U (U64 ~> F U64))))
  addLazy :: stk (F Unit) (U (F U64) ~> U (F U64) ~> F U64)

infixl 9 &&&

infixl 9 |||
