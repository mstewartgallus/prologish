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
import Prelude hiding (curry, id, uncurry, (.))

class (HasSum k, HasProduct k, HasCoexp k) => Mal k where
  u64 :: Word64 -> k Unit U64
  add :: k (U64 * U64) U64

  commuteSum :: k (a + b) (b + a)
  commuteSum = right ||| left

  factorIn :: k (v * (a + b)) ((v * a) + (v * b))
  factorIn = try (mal (right . first) &&& mal (commuteSum . (try (mal (right . first) &&& mal (commuteSum . second)))))

  factorOut :: k ((a * b) + (a * c)) (a * (b + c))
  factorOut = (first ||| first) &&& ((left . second) ||| (right . second))
