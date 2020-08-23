{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv.Sort
  ( Set,
    U,
    Unit,
    type (*),
    type (+),
    U64,
    Algebra,
    F,
    Void,
    I,
    type (&),
    type (~>)
  )
where

import Data.Typeable ((:~:) (..))

type Set = SetImpl

type Unit = 'Unit

type (*) = 'Product

infixl 0 *

type (+) = 'Sum

infixl 0 +

type U64 = 'U64

type Algebra = AlgebraImpl

type I = 'I

type Void = 'Void

type (~>) = 'Exp
infixr 9 ~>

type (&) = 'Asym

infixr 0 &

type U x = 'U x

type F x = x & I

data SetImpl = U Algebra | Unit | Void | Sum Set Set | Product Set Set | U64

data AlgebraImpl =  I | Exp Set Algebra | Asym Set Algebra
