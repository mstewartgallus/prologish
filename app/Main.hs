{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import AsEval
import qualified AsMal
import qualified AsTerm
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
import Term (Term)
import qualified Term.AsView as AsTermView
import Prelude hiding ((<*>))

main :: IO ()
main = do
  x <- Id.stream

  putStrLn "The Program"
  putStrLn (AsHoasView.view (bound x))

  putStrLn ""
  putStrLn "De-Bruijn"
  putStrLn (AsTermView.view (debruijn x))

  putStrLn ""
  putStrLn "Co-CCC"
  putStrLn (AsMalView.view (malP x))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result x))

type Type = (U64 * U64) -< Unit

program :: Hoas t => t Type
program =
  mal inferT $ \tuple ->
    tuple
      `isBoth` ( mal inferT $ \x -> x `isU64` 5,
                 mal inferT $ \y -> y `isU64` 3
               )

bound :: Bound t => Id.Stream -> t Type
bound str = bindPoints str program

debruijn :: Term k => Id.Stream -> k Type '[]
debruijn str = AsTerm.pointFree (bound str)

malP :: Mal k => Id.Stream -> k (AsMal.AsObject Type) Mal.Type.Void
malP str = AsMal.asMal (debruijn str)

compiled :: MonadCont m => Id.Stream -> Value m (AsMal.AsObject Type) -> m (Value m Mal.Type.Void)
compiled str = AsEval.asEval (malP str)

-- fixme... use and generate lists ?
result :: Id.Stream -> (Word64, Word64)
result str = flip runCont id $ do
  callCC $ \k -> do
    abs <- compiled str $ Coexp Coin $ \(Pair (Value64 x) (Value64 y)) -> k (x, y)
    case abs of
