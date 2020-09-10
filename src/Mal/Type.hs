{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.Type (T, Unit, Void, type (-<), type (*), type (+), type U64) where

type Void = 'Void
type Unit = 'Unit

type (-<) = 'Coexp

type (+) = 'Sum
type (*) = 'Prod

type U64 = 'U64

infixr 9 -<

infixl 0 +
infixl 0 *

data T = U64 | Unit | Void | Sum T T | Prod T T | Coexp T T
