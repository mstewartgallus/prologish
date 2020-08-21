module Main where

import AsBound
import AsCbpv
import AsLabeless
import AsReified
import AsVarless
import AsView
import Cbpv (Cbpv)
import Data.Word
import Exp
import Hoas
import qualified Id
import Lambda
import Product
import Type
import Prelude hiding ((<*>))

main :: IO ()
main = do
  putStrLn (view program)
  Id.Stream _ x y <- Id.stream
  putStrLn (view (compiled x))
  putStrLn (show (result y))

program :: (Lambda k, Hoas k) => k Unit U64
program = u64 42 `letBe` var inferT inferT $ \x ->
  u64 3 `letBe` var inferT inferT $ \y ->
    u64 3 `letBe` var inferT inferT $ \z ->
      add <*> z <*> (add <*> x <*> y)

compiled :: Lambda k => Id.Stream -> k Unit U64
compiled str = (removeLabels . removeVariables . bindPoints str) program

result :: Id.Stream -> Word64
result str = reify (compiled str)

cbpv :: Cbpv c d => Id.Stream -> c x (AsAlgebra U64)
cbpv str = toCbpv (compiled str)
