{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv.Sort
  ( Set,
    U,
    Unit,
    Void,
    type (*),
    type (+),
    U64,
    Algebra,
    F,
    Initial,
    type (&),
    type (~>)
  )
where

type Set = SetImpl

type Unit = 'Unit

type Void = 'Void

type (*) = 'Product

infixl 0 *

type (+) = 'Sum

infixl 0 +

type U64 = 'U64

type Algebra = AlgebraImpl

type Initial = 'Initial

type (~>) = 'Exp
infixr 9 ~>

type (&) = 'Asym

infixr 0 &

type U x = 'U x

type F x = x & Initial

data SetImpl = U Algebra | Unit | Void | Sum Set Set | Product Set Set | U64

data AlgebraImpl = Initial | Exp Set Algebra | Asym Set Algebra
