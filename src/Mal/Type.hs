{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.Type (T, Unit, Void, type (-<), type (*), type (+), type B, type U64) where

type Void = 'Void
type Unit = 'Unit

type (-<) = 'Coexp

type (+) = 'Sum
type (*) = 'Prod

type B = 'B
type U64 = 'U64

infixr 9 -<

infixl 0 +
infixl 0 *

data T = U64 | B | Unit | Void | Sum T T | Prod T T | Coexp T T
