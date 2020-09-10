{-# LANGUAGE DataKinds #-}

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

program :: Hoas t => t U64
program =
  u64 3 `letBe` \z ->
    add <*> z <*> z

bound :: Bound t => Id.Stream -> t U64
bound str = bindPoints str program

debruijn :: Term k => Id.Stream -> k '[] U64
debruijn str = AsTerm.pointFree (bound str)

compiled :: Lambda k => Id.Stream -> k Lambda.Type.Unit Lambda.Type.U64
compiled str = AsLambda.asLambda (debruijn str)

optimized :: Lambda k => Id.Stream -> k Lambda.Type.Unit Lambda.Type.U64
optimized str = optimize (compiled str)

mal :: Mal k => Id.Stream -> k (AsMal.AsOp Lambda.Type.U64) (AsMal.AsOp Lambda.Type.Unit)
mal str = AsMal.asMal (optimized str)
