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
import qualified Id
import LabelHoas
import Labels
import Lambda
import Product
import Type
import VarHoas
import Prelude hiding ((<*>))

main :: IO ()
main = do
  putStrLn "The Program"
  putStrLn (view program)

  Id.Stream _ x (Id.Stream _ y z) <- Id.stream

  putStrLn "Labeless Program"
  putStrLn (view (labeless y))

  putStrLn "Program Result"
  putStrLn (show (result z))

program :: (Lambda k, VarHoas k, LabelHoas k) => k Unit U64
program = u64 42 `letBe` var inferT inferT $ \x ->
  u64 3 `letBe` var inferT inferT $ \y ->
    u64 3 `letBe` var inferT inferT $ \z ->
      add <*> z <*> (add <*> x <*> y)

varless :: (Labels k, Lambda k) => Id.Stream -> k Unit U64
varless str = (removeVariables . bindPoints str) program

labeless :: Lambda k => Id.Stream -> k Unit U64
labeless str = removeLabels (varless str)

result :: Id.Stream -> Word64
result str = reify (labeless str)

cbpv :: Cbpv c d => Id.Stream -> c x (AsAlgebra U64)
cbpv str = toCbpv (labeless str)
