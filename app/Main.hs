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
  putStrLn "Varless Program"
  putStrLn (view (varless y))

  putStrLn ""
  putStrLn "Labeless Program"
  putStrLn (view (labeless y))

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

varless :: (Labels k, Lambda k) => Id.Stream -> k Unit U64
varless str = removeVariables (bound str)

labeless :: Lambda k => Id.Stream -> k Unit U64
labeless str = removeLabels (varless str)

result :: Id.Stream -> Word64
result str = reify (labeless str)

cbpv :: Cbpv c d => Id.Stream -> c x (AsAlgebra U64)
cbpv str = toCbpv (labeless str)
