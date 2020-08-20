module Main where

import Data.Word
import Eval
import Exp
import FromHoas
import Hoas
import Lambda
import Product
import Sum
import Type
import View
import Prelude hiding ((<*>))

main :: IO ()
main = do
  putStrLn (view program)
  putStrLn (view compiled)
  putStrLn (show result)

program :: (Lambda k, Hoas k) => Value k U64
program = u64 42 `letBe` var inferT $ \x ->
  u64 3 `letBe` var inferT $ \y ->
    u64 3 `letBe` var inferT $ \z ->
      add <*> z <*> (add <*> x <*> y)

compiled :: Lambda k => Value k U64
compiled = fromHoas program

result :: Word64
result = execute compiled
