{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.Type (T, Void, Unit, type (-<), type (*), type (+), type U64) where

type Void = 'Void

type Unit = 'Unit

type (-<) = 'Coexp

type (*) = 'Product

type (+) = 'Sum

type U64 = 'U64

infixr 9 -<

infixl 0 *

infixl 0 +

data T = U64 | Void | Unit | Sum T T | Product T T | Coexp T T
