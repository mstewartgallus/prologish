{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module AsEval (Expr, asEval, Value (..)) where

import Control.Category
import Control.Monad.Cont
import Data.Kind
import qualified Data.Void as Void
import Data.Word
import Mal
import Mal.HasCoexp
import Mal.HasSum
import Mal.Type
import Prelude hiding (Either (..), id, (.))

asEval :: Expr m a b -> Value m a -> m (Value m b)
asEval (E x) = x

data family Value (m :: Type -> Type) (a :: T)

data instance Value m (a + b) = Left (Value m a) | Right (Value m b)

data instance Value m (a -< b) = Coexp (Value m b) (Value m a -> m Void.Void)

data instance Value m Void = Absurd (m Void.Void)

newtype instance Value m U64 = Value64 Word64

newtype Expr m a b = E (Value m a -> m (Value m b))

instance Monad m => Category (Expr m) where
  id = E pure
  E f . E g = E $ \x -> do
    y <- g x
    f y

instance Monad m => HasSum (Expr m) where
  absurd = E $ \(Absurd x) -> do
    abs <- x
    Void.absurd abs

  E f ||| E g = E $ \x -> case x of
    Left l -> f l
    Right r -> g r
  left = E (pure . Left)
  right = E (pure . Right)

instance MonadCont m => HasCoexp (Expr m) where
  throw (E f) = E $ \(Coexp x k) -> do
    y <- f x
    case y of
      Left l -> do
        abs <- k l
        Void.absurd abs
      Right r -> pure r
  try (E f) = E $ \b -> callCC $ \k -> do
    env <- f $ Coexp b $ \x -> k (Left x)
    pure (Right env)

instance MonadCont m => Mal (Expr m)
