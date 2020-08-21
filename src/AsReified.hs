{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsReified (Eval (..), reify, Reify) where

import Control.Category
import qualified Data.Void as Void
import Data.Word
import Exp
import Lambda
import Product
import Sum
import Type
import Prelude hiding ((.), id)

reify :: Eval Unit a -> Reify a
reify (Eval f) = f ()

newtype Eval a b = Eval (Reify a -> Reify b)

type family Reify a where
  Reify Unit = ()
  Reify Void = Void.Void
  Reify (a * b) = (Reify a, Reify b)
  Reify (a + b) = Either (Reify a) (Reify b)
  Reify (a ~> b) = Reify a -> Reify b
  Reify U64 = Word64

instance Category Eval where
  id = Eval id
  Eval f . Eval g = Eval (f . g)

instance Product Eval where
  unit = Eval $ const ()
  Eval x # Eval y = Eval $ \env -> (x env, y env)
  first = Eval fst
  second = Eval snd

instance Sum Eval where
  absurd = Eval Void.absurd
  Eval f ! Eval x = Eval $ \env -> case env of
    Left env' -> f env'
    Right env' -> x env'
  left = Eval Left
  right = Eval Right

instance Exp Eval where
  lambda (Eval f) = Eval $ \env x -> f (env, x)
  eval = Eval $ \(f, x) -> f x

instance Lambda Eval where
  u64 x = Eval $ const $ x
  add = Eval $ const $ \x y -> x + y
