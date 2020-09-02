{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Type (T, Void, Unit, type (~>), type (*), type (+), type U64) where

import Data.Kind (Type)
import Data.Typeable ((:~:) (..))

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
