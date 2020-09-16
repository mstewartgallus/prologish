{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import AsEval
import qualified AsTerm
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
import Mal.AsView
import qualified Mal.Type
import Prelude hiding ((<*>))

main :: IO ()
main = do
  x <- Id.stream

  putStrLn "The Program"
  putStrLn (AsHoasView.view (bound x))

  putStrLn ""
  putStrLn "Point-Free Program"
  putStrLn (view (malP x))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result x))

type TYPE = (U64 -< Unit) -< Unit

program :: Hoas t => t Unit TYPE
program =
  unit |= \k ->
    k ! u64 3

bound :: Bound t => Id.Stream -> t Unit TYPE
bound str = bindPoints str program

malP :: Mal k => Id.Stream -> k Mal.Type.Unit (AsTerm.AsObject TYPE)
malP str = AsTerm.pointFree (bound str)

compiled :: MonadCont m => Id.Stream -> m (Value m (AsTerm.AsObject TYPE))
compiled str = AsEval.asEval (malP str) Coin

result :: Id.Stream -> Word64
result str = flip runCont id $
  callCC $ \k -> do
    Coin :- c <- compiled str
    abs <-
      c $
        Coin :- \(Value64 z) -> k z
    Void.absurd abs
