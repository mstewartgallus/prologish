{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Mal.Type (T, Void, type (-<), type (+), type U64) where

type Void = 'Void

type (-<) = 'Coexp

type (+) = 'Sum

type U64 = 'U64

infixr 9 -<

infixl 0 +

data T = U64 | Void | Sum T T | Coexp T T
