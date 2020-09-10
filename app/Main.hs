{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import AsEval
import qualified AsMal
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
  putStrLn "De-Bruijn Program"
  putStrLn (AsTermView.view (debruijn x))

  putStrLn ""
  putStrLn "Flipped Program"
  putStrLn (AsMalView.view (mal x))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result x 5))

type Type = U64 -< U64 -< U64

program :: Hoas t => t Type
program =
  kont inferT $ \x ->
    kont inferT $ \_ ->
      x

bound :: Bound t => Id.Stream -> t Type
bound str = bindPoints str program

debruijn :: Term k => Id.Stream -> k Type '[]
debruijn str = AsTerm.pointFree (bound str)

mal :: Mal k => Id.Stream -> k (AsMal.AsObject Type) Mal.Type.Void
mal str = AsMal.asMal (debruijn str)

compiled :: MonadCont m => Id.Stream -> Value m (AsMal.AsObject Type) -> m (Value m Mal.Type.Void)
compiled str = AsEval.asEval (mal str)

result :: Id.Stream -> Word64 -> Either Word64 Word64
result str input = flip runCont id $
  callCC $ \k -> do
    Absurd go <- compiled str $ Coexp (Coexp (Value64 input) $ \(Value64 x) -> k (Prelude.Left x)) $ \(Value64 y) -> k (Prelude.Right y)
    abs <- go
    Void.absurd abs
