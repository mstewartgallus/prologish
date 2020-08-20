{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Eval (Eval (..), execute, Value (..)) where

import Control.Category
import Data.Word
import Lam
import Type
import Prelude hiding ((.), id)

execute :: Eval Unit a -> Value a
execute (Eval f) = f ()

newtype Eval a b = Eval (Value a -> Value b)

type family Value a where
  Value Unit = ()
  Value U64 = Word64
  Value (a * b) = (Value a, Value b)
  Value (a + b) = Either (Value a) (Value b)
  Value (a ~> b) = Value a -> Value b

instance Category Eval where
  id = Eval id
  Eval f . Eval g = Eval (f . g)

instance Lam Eval where
  Eval x # Eval y = Eval $ \env -> (x env, y env)
  first = Eval fst
  second = Eval snd

  Eval f ! Eval x = Eval $ \env -> case env of
    Left env' -> f env'
    Right env' -> x env'
  left = Eval Left
  right = Eval Right

  lambda (Eval f) = Eval $ \env x -> f (env, x)
  unlambda (Eval f) = Eval $ \(env, x) -> f env x

  u64 x = Eval $ const $ x
  add = Eval $ const $ \x y -> x + y
