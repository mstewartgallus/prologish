{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import AsEval
import qualified AsTerm
import AsView
import Control.Category
import Control.Monad.Cont
import Data.Dynamic
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Void as Void
import Data.Word
import HasProduct
import HasSum
import Hoas
import Hoas.AsBound
import Hoas.Bound (Bound)
import qualified Id
import Mal (Mal)
import Type
import Prelude hiding (id, succ, (.), (<*>))

main :: IO ()
main = do
  x <- Id.stream

  putStrLn "The Program"
  putStrLn (view (bound x))

  putStrLn ""
  putStrLn "Point-Free Program"
  putStrLn (view (malP x))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result x))

type TYPE = U64

program :: Hoas t => t (U64 -< Unit) Void
program = st inferT $ \x ->
  (x `constrain` (st inferT $ \y -> y <<< u64 5))
    `amb` (x `constrain` (st inferT $ \y -> y <<< u64 5))

full :: Hoas t => t (TYPE -< Unit) Void
full = program

bound :: Bound t => Id.Stream -> t (TYPE -< Unit) Void
bound str = bindPoints str full

malP :: Mal k => Id.Stream -> k (TYPE -< Unit) Void
malP str = AsTerm.pointFree (bound str)

compiled :: MonadCont m => Id.Stream -> m (Value m TYPE)
compiled str = callCC $ \k -> do
  abs <- AsEval.asEval (malP str) $ Coin :- \x -> k x
  case abs of

result :: Id.Stream -> Word64
result str = flip runCont id $ do
  val <- compiled str
  case val of
    Value64 x -> pure x
