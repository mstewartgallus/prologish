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
class Cbpv stk dta | stk -> dta, dta -> stk where
  thunk ::
    (stk (F x) -> stk y) ->
    dta x ->
    dta (U y)
  force ::
    (dta x -> dta (U y)) ->
    stk (F x) ->
    stk y

  return ::
    (dta env -> dta a) ->
    stk (F env) ->
    stk (F a)
  to ::
    (stk (env & k) -> stk (F a)) ->
    (stk (F (env * a)) -> stk b) ->
    (stk (env & k) -> stk b)

  unit :: dta x -> dta Unit
  (&&&) :: (dta env -> dta a) -> (dta env -> dta b) -> (dta env -> dta (a * b))
  first :: dta (a * b) -> dta a
  second :: dta (a * b) -> dta b

  absurd :: dta Void -> dta x
  (|||) :: (dta a -> dta c) -> (dta b -> dta c) -> (dta (a + b) -> dta c)
  left :: dta a -> dta (a + b)
  right :: dta b -> dta (a + b)

  assocOut :: stk (a & (b & c)) -> stk ((a * b) & c)
  assocIn :: stk ((a * b) & c) -> stk (a & (b & c))

  curry :: (stk (a & env) -> stk b) -> (stk env -> stk (a ~> b))
  uncurry :: (stk env -> stk (a ~> b)) -> (stk (a & env) -> stk b)

  u64 :: Word64 -> dta x -> dta U64

  -- fixme.. have optimized version...
  add :: dta Unit -> dta (U (U64 ~> F (U (U64 ~> F U64))))
  addLazy :: stk (F Unit) -> stk (U (F U64) ~> U (F U64) ~> F U64)

infixl 9 &&&

infixl 9 |||
