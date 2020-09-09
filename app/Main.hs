{-# LANGUAGE DataKinds #-}

module Main where

import AsCallByName
import qualified AsLambda
import qualified AsPointy
import qualified AsTerm
import Cbpv (Cbpv)
import qualified Cbpv.AsEval as AsEval
import qualified Cbpv.AsView as AsViewCbpv
import qualified Cbpv.Sort
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
import Pointy (Pointy)
import qualified Pointy.AsView as AsPointyView
import Term (Term)
import qualified Term.AsView as AsTermView
import Prelude hiding ((<*>))

main :: IO ()
main = do
  Id.Stream _ (Id.Stream _ x (Id.Stream _ y v)) (Id.Stream _ z (Id.Stream _ w u)) <- Id.stream

  putStrLn "The Program"
  putStrLn (AsHoasView.view (bound x))

  putStrLn ""
  putStrLn "Pointful Style"
  putStrLn (AsPointyView.view (fn z))

  putStrLn ""
  putStrLn "De-Bruijn Program"
  putStrLn (AsTermView.view (debruijn y))

  putStrLn ""
  putStrLn "Point-Free Program"
  putStrLn (view (compiled z))

  putStrLn ""
  putStrLn "Optimized Program"
  putStrLn (view (optimized w))

  putStrLn ""
  putStrLn "Cbpv Program"
  putStrLn (AsViewCbpv.view (cbpv u))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result v))

program :: Hoas t => t U64
program =
  u64 3 `letBe` \z ->
    add <*> z <*> z

bound :: Bound t => Id.Stream -> t U64
bound str = bindPoints str program

fn :: Pointy t => Id.Stream -> t Lambda.Type.Unit -> t Lambda.Type.U64
fn str = AsPointy.pointFree (bound str)

debruijn :: Term k => Id.Stream -> k '[] U64
debruijn str = AsTerm.pointFree (bound str)

compiled :: Lambda k => Id.Stream -> k Lambda.Type.Unit Lambda.Type.U64
compiled str = AsLambda.asLambda (debruijn str)

optimized :: Lambda k => Id.Stream -> k Lambda.Type.Unit Lambda.Type.U64
optimized str = optimize (compiled str)

cbpv :: Cbpv c d => Id.Stream -> d (Cbpv.Sort.U (Cbpv.Sort.F Cbpv.Sort.Unit)) (Cbpv.Sort.U (AsAlgebra Lambda.Type.U64))
cbpv str = toCbpv (optimized str)

result :: Id.Stream -> Word64
result str = AsEval.reify (cbpv str)
