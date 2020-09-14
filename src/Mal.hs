{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- |
--
-- Export the final type class of the simple lambda calculus language.
-- Here we finish the Lambda type class off with some basic operations on
-- integers.
module Mal (Mal (..)) where

import Control.Category
import Data.Word (Word64)
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type
import Prelude hiding ((.))

class (HasProduct k, HasCoexp k) => Mal k where
  pick :: k B (Unit + Unit)

  u64 :: Word64 -> k Unit U64
  add :: k env U64 -> k env U64 -> k env U64

  factorIn :: k (v * (a + b)) ((v * a) + (v * b))
  factorIn = try (mal (right . first) &&& mal (commuteSum . (try (mal (right . first) &&& mal (commuteSum . second)))))

  factorOut :: k ((a * b) + (a * c)) (a * (b + c))
  factorOut = (first ||| first) &&& ((left . second) ||| (right . second))
