{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module FromHoas (Expr, fromHoas) where

import Control.Category
import Control.Monad.State
import Data.Typeable ((:~:) (..))
import Hoas
import Lam
import Type
import Prelude hiding ((.), id)

fromHoas :: Lam k => Expr (Varless (Labeless k)) a b -> k a b
fromHoas = removeLabels . removeVars . bind

data Var a = Var (ST a) Int

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

data Label a = Label (ST a) Int

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

newtype Expr k (a :: T) (b :: T) = Expr {unExpr :: State Int (k a b)}

bind :: Expr k env a -> k env a
bind (Expr x) = evalState x 0

instance Vars k => Hoas (Expr k) where
  var t f = Expr $ do
    n <- fresh
    let v = Var t n
    body <- unExpr (f (Expr $ pure (mkVar v)))
    pure (bindVar v body)
  label t f = Expr $ do
    n <- fresh
    let v = Label t n
    body <- unExpr (f (Expr $ pure (mkLabel v)))
    pure (bindLabel v body)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  return n

instance Category k => Category (Expr k) where
  id = Expr $ pure id
  Expr f . Expr g = Expr $ liftM2 (.) f g

instance Lam k => Lam (Expr k) where
  Expr f # Expr g = Expr $ liftM2 (#) f g
  first = Expr $ pure first
  second = Expr $ pure second

  Expr f ! Expr g = Expr $ liftM2 (!) f g
  left = Expr $ pure left
  right = Expr $ pure right

  lambda (Expr f) = Expr $ liftM lambda f
  eval = Expr $ pure eval

  u64 x = Expr $ pure (u64 x)
  add = Expr $ pure add

class Lam k => Labels k where
  mkLabel :: Label a -> k a x
  bindLabel :: Label b -> k env a -> k env (a + b)

class Labels k => Vars k where
  mkVar :: Var a -> k x a
  bindVar :: Var a -> k env b -> k (env * a) b

data Varless k a b where
  VarlessVar :: Var a -> Varless k x a
  VarlessLabel :: Label a -> Varless k a x
  VarlessCompose :: Varless k b c -> Varless k a b -> Varless k a c
  VarlessFactor :: Varless k c a -> Varless k c b -> Varless k c (a * b)
  VarlessFactorSum :: Varless k a c -> Varless k b c -> Varless k (a + b) c
  VarlessLambda :: Varless k (env * a) b -> Varless k env (a ~> b)
  VarlessPure :: k a b -> Varless k a b

instance Category k => Category (Varless k) where
  id = VarlessPure id
  (.) = VarlessCompose

instance Labels k => Labels (Varless k) where
  mkLabel lbl = VarlessPure (mkLabel lbl)
  bindLabel = undefined

instance Labels k => Vars (Varless k) where
  mkVar = VarlessVar
  bindVar = abstractVar

instance Lam k => Lam (Varless k) where
  f # g = VarlessFactor f g
  first = VarlessPure first
  second = VarlessPure second

  f ! g = VarlessFactorSum f g
  left = VarlessPure left
  right = VarlessPure right

  lambda f = VarlessLambda f
  eval = VarlessPure eval

  u64 x = VarlessPure (u64 x)
  add = VarlessPure add

removeVars :: Lam k => Varless k a b -> k a b
removeVars expr = case expr of
  VarlessPure k -> k
  VarlessCompose f g -> removeVars f . removeVars g
  VarlessFactor f g -> removeVars f # removeVars g
  VarlessLambda f -> lambda (removeVars f)

abstractVar :: Lam k => Var a -> Varless k env b -> Varless k (env * a) b
abstractVar m expr = case expr of
  p@(VarlessVar n) -> case m `eqVar` n of
    Just Refl -> VarlessPure second
    Nothing -> p . VarlessPure first
  VarlessPure k -> VarlessPure k . VarlessPure first
  VarlessCompose f g -> f' . VarlessFactor g' (VarlessPure second)
    where
      f' = abstractVar m f
      g' = abstractVar m g
  VarlessFactor f g -> VarlessFactor f' g'
    where
      f' = abstractVar m f
      g' = abstractVar m g
  VarlessLambda f -> VarlessLambda (f' . flp)
    where
      flp = VarlessFactor (VarlessFactor getenv geta) getb
      geta = VarlessPure second
      getb = VarlessPure second . VarlessPure first
      getenv = VarlessPure first . VarlessPure first
      f' = abstractVar m f

data Labeless k a b where
  LabelessLabel :: Label a -> Labeless k a x
  LabelessCompose :: Labeless k b c -> Labeless k a b -> Labeless k a c
  LabelessFactor :: Labeless k c a -> Labeless k c b -> Labeless k c (a * b)
  LabelessFactorSum :: Labeless k a c -> Labeless k b c -> Labeless k (a + b) c
  LabelessLambda :: Labeless k (env * a) b -> Labeless k env (a ~> b)
  LabelessPure :: k a b -> Labeless k a b

instance Category k => Category (Labeless k) where
  id = LabelessPure id
  (.) = LabelessCompose

instance Lam k => Labels (Labeless k) where
  mkLabel = LabelessLabel
  bindLabel = abstractLabel

instance Lam k => Lam (Labeless k) where
  f # g = LabelessFactor f g
  first = LabelessPure first
  second = LabelessPure second

  f ! g = LabelessFactorSum f g
  left = LabelessPure left
  right = LabelessPure right

  lambda f = LabelessLambda f
  eval = LabelessPure eval

  u64 x = LabelessPure (u64 x)
  add = LabelessPure add

abstractLabel :: Lam k => Label a -> Labeless k env b -> Labeless k env (b + a)
abstractLabel m expr = case expr of
  p@(LabelessLabel n) -> case m `eqLabel` n of
    Just Refl -> LabelessPure right
    Nothing -> LabelessPure left . p
  LabelessPure k -> LabelessPure left . LabelessPure k
  LabelessCompose f g -> LabelessFactorSum f' (LabelessPure right) . g'
    where
      f' = abstractLabel m f
      g' = abstractLabel m g

removeLabels :: Lam k => Labeless k a b -> k a b
removeLabels expr = case expr of
  LabelessPure k -> k
  LabelessCompose f g -> removeLabels f . removeLabels g
  LabelessFactor f g -> removeLabels f # removeLabels g
  LabelessLambda f -> lambda (removeLabels f)
