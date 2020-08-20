{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda (Lambda (..)) where

import Data.Word (Word64)
import Exp
import Product
import Sum
import Type
import Prelude hiding ((.), id)

class (Sum k, Product k, Exp k) => Lambda k where
  u64 :: Word64 -> Value k U64
  add :: Value k (U64 ~> U64 ~> U64)
