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
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Void as Void
import Data.Word
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
  putStrLn (show (result x (Prelude.Left Prelude.True)))

type TYPE = B -< B -< (B + B)

program :: Hoas t => t TYPE Void
program =
  st inferT $ \s0 ->
    st inferT $ \s1 ->
      s0 ||| s1

bound :: Bound t => Id.Stream -> t TYPE Void
bound str = bindPoints str program

malP :: Mal k => Id.Stream -> k TYPE Void
malP str = AsTerm.pointFree (bound str)

compiled :: MonadCont m => Id.Stream -> Value m TYPE -> m (Value m Void)
compiled str = AsEval.asEval (malP str)

result :: Id.Stream -> Either Bool Bool -> Bool
result str x = flip runCont id $
  callCC $ \k -> do
    abs <- compiled str $ from x :- (\z -> k (toBool z)) :- (\z -> k (toBool z))
    case abs of

from :: Either Bool Bool -> Value m (B + B)
from (Prelude.Left x) = AsEval.Left (fromBool x)
from (Prelude.Right x) = AsEval.Right (fromBool x)

fromBool :: Bool -> Value m B
fromBool expr = if expr then AsEval.True else AsEval.False

toBool :: Value m B -> Bool
toBool expr = case expr of
  AsEval.True -> Prelude.True
  AsEval.False -> Prelude.False
