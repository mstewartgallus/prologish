{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Type (T, Void, Unit, type (~>), type (*), type (+), type U64) where

type Void = 'Void

type Unit = 'Unit

type (~>) = 'Exp

type (*) = 'Product

type (+) = 'Sum

type U64 = 'U64

infixr 9 ~>

infixl 0 *

infixl 0 +

data T = U64 | Void | Unit | Sum T T | Product T T | Exp T T
