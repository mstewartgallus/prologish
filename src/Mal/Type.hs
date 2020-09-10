{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Mal.Type (T, Unit, Void, type (-<), type (+), type U64) where

type Void = 'Void
type Unit = 'Unit

type (-<) = 'Coexp

type (+) = 'Sum

type U64 = 'U64

infixr 9 -<

infixl 0 +

data T = U64 | Unit | Void | Sum T T | Coexp T T
