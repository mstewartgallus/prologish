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
import Prelude hiding (Bool (..), Either (..), (<*>))

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

type Type = B |- (U64 * U64)

program :: Hoas t => t Type
program =
  mal inferT $ \tuple ->
    pick $
      tuple
        `isBoth` ( mal inferT $ \a ->
                     tuple
                       `isBoth` ( mal inferT $ \_ -> (a `isU64` 13) ||| (a `isU64` 43),
                                  mal inferT $ \b -> (a `isU64` 90) ||| (b `isU64` 18)
                                ),
                   mal inferT $ \_ ->
                     tuple
                       `isBoth` ( mal inferT $ \a -> (a `isU64` 3) ||| (a `isU64` 4),
                                  mal inferT $ \b -> (b `isU64` 9) ||| (b `isU64` 8)
                                )
                 )

bound :: Bound t => Id.Stream -> t Type
bound str = bindPoints str program

debruijn :: Term k => Id.Stream -> k Type '[]
debruijn str = AsTerm.pointFree (bound str)

malP :: Mal k => Id.Stream -> k (AsMal.AsObject Type) Mal.Type.Void
malP str = AsMal.asMal (debruijn str)

compiled :: MonadCont m => Id.Stream -> Value m (AsMal.AsObject Type) -> m (Value m Mal.Type.Void)
compiled str = AsEval.asEval (malP str)

result :: Id.Stream -> [(Word64, Word64)]
result str = flip runCont id $ do
  forM [True, False] $ \input ->
    callCC $ \k -> do
      abs <-
        compiled str $
          input :- (\(Value64 x ::: Value64 y) -> k (x, y))
      case abs of
