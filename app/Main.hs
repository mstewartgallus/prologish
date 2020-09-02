module Main where

import AsCallByName
import AsPointFree
import Cbpv (Cbpv)
import qualified Cbpv.AsEval as AsEval
import qualified Cbpv.AsView as AsViewCbpv
import qualified Cbpv.Sort
import Data.Word
import qualified Id
import Lambda (Lambda)
import qualified Lambda.AsConcrete as AsConcrete
import Lambda.AsView
import Lambda.Optimize
import qualified Lambda.Type
import Term
import Term.AsBoundTerm
import qualified Term.AsView as AsTermView
import Term.Bound (Bound)
import Term.Type
import Prelude hiding ((<*>))

main :: IO ()
main = do
  Id.Stream _ (Id.Stream _ x (Id.Stream _ y v)) (Id.Stream _ z (Id.Stream _ w u)) <- Id.stream

  putStrLn "The Program"
  putStrLn (AsTermView.view (bound x))

  putStrLn ""
  putStrLn "Point-Free Program"
  putStrLn (view (compiled y))

  putStrLn ""
  putStrLn "Optimized Program"
  putStrLn (view (optimized w))

  putStrLn ""
  putStrLn "Cbpv Program"
  putStrLn (AsViewCbpv.view (cbpv u))

  putStrLn ""
  putStrLn "Result"
  putStrLn (show (result v))

program :: Term t => t U64
program =
  u64 42 `letBe` \x ->
    u64 3 `letBe` \y ->
      u64 3 `letBe` \z ->
        add <*> z <*> (add <*> x <*> y)

bound :: Bound t => Id.Stream -> t U64
bound str = bindPoints str program

compiled :: Lambda k => Id.Stream -> k Lambda.Type.Unit Lambda.Type.U64
compiled str = pointFree (bound str)

optimized :: Lambda k => Id.Stream -> k Lambda.Type.Unit Lambda.Type.U64
optimized str = AsConcrete.abstract (optimize (compiled str))

cbpv :: Cbpv c d => Id.Stream -> d (Cbpv.Sort.U (Cbpv.Sort.F Cbpv.Sort.Unit)) -> d (Cbpv.Sort.U (AsAlgebra Lambda.Type.U64))
cbpv str = toCbpv (optimized str)

result :: Id.Stream -> Word64
result str = AsEval.reify (cbpv str)
