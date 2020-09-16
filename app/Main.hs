{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import AsEval
import qualified AsTerm
import Control.Category
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
import Prelude hiding (id, (.), (<*>))

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

type TYPE = U64 -< (U64 * U64)

program :: Hoas t => t TYPE Void
program = mal $
  letLabel inferT $ \k ->
    (add `try` k) <<< (first id &&& second id)

bound :: Bound t => Id.Stream -> t TYPE Void
bound str = bindPoints str program

malP :: Mal k => Id.Stream -> k (AsTerm.AsObject TYPE) Mal.Type.Void
malP str = AsTerm.pointFree (bound str)

compiled :: MonadCont m => Id.Stream -> Value m (AsTerm.AsObject TYPE) -> m (Value m Mal.Type.Void)
compiled str = AsEval.asEval (malP str)

result :: Id.Stream -> Word64
result str = flip runCont id $
  callCC $ \k -> do
    abs <- compiled str $ (Value64 2 ::: Value64 4) :- \(Value64 z) -> k z
    case abs of
