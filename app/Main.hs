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
  putStrLn (show (result x))

type Type = Unit |- (U64 |- U64)

program :: Hoas t => t Type
program =
  kont inferT unit $ \x ->
    jump inferT x $ \x' -> x' `add` u64 9

bound :: Bound t => Id.Stream -> t Type
bound str = bindPoints str program

malP :: Mal k => Id.Stream -> k Mal.Type.Unit (AsMal.AsObject Type)
malP str = AsMal.asMal (bound str)

compiled :: MonadCont m => Id.Stream -> m (Value m (AsMal.AsObject Type))
compiled str = AsEval.asEval (malP str) Coin

result :: Id.Stream -> Word64
result str = flip runCont id $
  callCC $ \k -> do
    Coin :- c <- compiled str
    abs <-
      c $
        Value64 5 :- \(Value64 x) -> do
          k x
    Void.absurd abs
