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
import Global
import HasCoexp
import HasProduct
import HasSum
import Type
import Prelude hiding (curry, id, uncurry, (.))

class (HasSum k, HasProduct k, HasCoexp k) => Mal k where
  true :: k Unit B
  false :: k Unit B

  amb :: k x Void -> k x Void -> k x Void

  u64 :: Word64 -> k Unit U64

  global :: Global a b -> k a b

  commuteSum :: k (a + b) (b + a)
  commuteSum = right ||| left

  factorIn :: k (v * (a + b)) ((v * a) + (v * b))
  factorIn = try (mal (right . first) &&& mal (commuteSum . (try (mal (right . first) &&& mal (commuteSum . second)))))

  factorOut :: k ((a * b) + (a * c)) (a * (b + c))
  factorOut = (first ||| first) &&& ((left . second) ||| (right . second))
