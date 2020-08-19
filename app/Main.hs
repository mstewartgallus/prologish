module Main where

import Data.Word
import Eval
import Expr
import Lam
import Type
import Prelude hiding ((<*>))

main :: IO ()
main = do
  putStrLn (view program)
  putStrLn (show result)

program :: Lam k => Object (Expr k) U64
program = u64 42 `letBe` var $ \x ->
  u64 3 `letBe` var $ \y ->
    u64 3 `letBe` var $ \z ->
      add <*> z <*> (add <*> x <*> y)

compiled :: Lam k => Object k U64
compiled = compile program

result :: Word64
result = execute compiled
