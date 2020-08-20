{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsEval (reify, Code, Data, Constant, Effect) where

import Cbpv
import Control.Category
import qualified Data.Void as Void
import Data.Word
import Sort
import Prelude hiding ((.), id)

reify :: Code Initial a -> Effect a
reify (Code f) = f ()

newtype Data a b = Data (Constant a -> Constant b)

newtype Code a b = Code (Effect a -> Effect b)

type family Constant a where
  Constant (U a) = Effect a
  Constant Unit = ()
  Constant (a * b) = (Constant a, Constant b)
  Constant (a + b) = Either (Constant a) (Constant b)
  Constant U64 = Word64

type family Effect a where
  Effect (F a) = Constant a
  Effect Initial = ()
  Effect Void = Void.Void
  Effect (a ~> b) = Constant a -> Effect b

instance Category Data where
  id = Data id
  Data f . Data g = Data (f . g)

instance Category Code where
  id = Code id
  Code f . Code g = Code (f . g)

instance Cbpv Code Data where
  thunk (Code f) = Data f
  force (Data f) = Code f

  initial = Code $ const ()

  absurd = Code Void.absurd

  Data x ! Data y = Data $ \env -> case env of
    Left l -> x l
    Right r -> y r
  left = Data Left
  right = Data Right

  unit = Data $ const ()

  Data x # Data y = Data $ \env -> (x env, y env)
  first = Data fst
  second = Data snd

  u64 x = Data $ const x
  add = Code $ const $ \x y -> x + y
