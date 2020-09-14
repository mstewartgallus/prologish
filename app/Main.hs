{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import AsEval
import qualified AsMal
import Control.Monad
import Control.Monad.Cont
import qualified Data.Void as Void
import Data.Word
import Hoas
import Hoas.AsBound
import qualified Hoas.AsView as AsHoasView
import Hoas.Bound (Bound)
import Hoas.Type
import qualified Id
import Mal (Mal)
import qualified Mal.AsView as AsMalView
import qualified Mal.Type
import Prelude hiding (Bool (..), Either (..), (<*>))

main :: IO ()
main = do
  x <- Id.stream

  putStrLn "The Program"
  putStrLn (AsHoasView.view (bound x))

  putStrLn ""
  putStrLn "Co-CCC"
  putStrLn (AsMalView.view (malP x))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result x 4 9))

type Type = Unit |- ((U64 * U64) |- U64)

program :: Hoas t => t Type
program =
  unit |= \x ->
    x ! \x' ->
      first x' `add` second x'

bound :: Bound t => Id.Stream -> t Type
bound str = bindPoints str program

malP :: Mal k => Id.Stream -> k Mal.Type.Unit (AsMal.AsObject Type)
malP str = AsMal.asMal (bound str)

compiled :: MonadCont m => Id.Stream -> m (Value m (AsMal.AsObject Type))
compiled str = AsEval.asEval (malP str) Coin

result :: Id.Stream -> Word64 -> Word64 -> Word64
result str x y = flip runCont id $
  callCC $ \k -> do
    Coin :- c <- compiled str
    abs <-
      c $
        (Value64 x ::: Value64 y) :- \(Value64 z) -> k z
    Void.absurd abs
