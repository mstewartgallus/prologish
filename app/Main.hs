{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified AsLambda
import qualified AsMal
import qualified AsTerm
import Data.Word
import Hoas
import Hoas.AsBound
import qualified Hoas.AsView as AsHoasView
import Hoas.Bound (Bound)
import Hoas.Type
import qualified Id
import Lambda (Lambda)
import Lambda.AsOptimized
import Lambda.AsView
import qualified Lambda.Type
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
  putStrLn "Point-Free Program"
  putStrLn (view (compiled x))

  putStrLn ""
  putStrLn "Optimized Program"
  putStrLn (view (optimized x))

  putStrLn ""
  putStrLn "Flipped Program"
  putStrLn (AsMalView.view (mal x))

type Type = U64 ~> U64 ~> U64

program :: Hoas t => t Type
program =
  lam inferT $ \x ->
    lam inferT $ \_ ->
      x

bound :: Bound t => Id.Stream -> t Type
bound str = bindPoints str program

debruijn :: Term k => Id.Stream -> k '[] Type
debruijn str = AsTerm.pointFree (bound str)

compiled :: Lambda k => Id.Stream -> k Lambda.Type.Unit (AsLambda.AsObject Type)
compiled str = AsLambda.asLambda (debruijn str)

optimized :: Lambda k => Id.Stream -> k Lambda.Type.Unit (AsLambda.AsObject Type)
optimized str = optimize (compiled str)

mal :: Mal k => Id.Stream -> k (AsMal.AsObject Type) Mal.Type.Void
mal str = AsMal.asMal (debruijn str)
