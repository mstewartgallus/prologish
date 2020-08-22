module Main where

import AsBound
import AsCbpv
import AsPointFree
import AsReified
import AsView
import Cbpv (Cbpv)
import Data.Word
import Exp
import Hoas
import qualified Id
import Labels
import Lambda
import Product
import Type
import Vars
import Prelude hiding ((<*>))

main :: IO ()
main = do
  Id.Stream _ x (Id.Stream _ y z) <- Id.stream

  putStrLn "The Program"
  putStrLn (view (bound x))

  putStrLn ""
  putStrLn "Point-Free Program"
  putStrLn (view (compiled y))

  putStrLn ""
  putStrLn "Program Result"
  putStrLn (show (result z))

program :: (Lambda k, Hoas k) => k Unit U64
program = u64 42 `letBe` var inferT inferT $ \x ->
  u64 3 `letBe` var inferT inferT $ \y ->
    u64 3 `letBe` var inferT inferT $ \z ->
      add <*> z <*> (add <*> x <*> y)

bound :: (Labels k, Vars k, Lambda k) => Id.Stream -> k Unit U64
bound str = bindPoints str program

compiled :: Lambda k => Id.Stream -> k Unit U64
compiled str = pointFree (bound str)

result :: Id.Stream -> Word64
result str = reify (compiled str)

cbpv :: Cbpv c d => Id.Stream -> c x (AsAlgebra U64)
cbpv str = toCbpv (compiled str)
