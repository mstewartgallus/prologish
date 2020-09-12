{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsTerm (PointFree, pointFree) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import qualified Hoas.Bound as Bound
import Hoas.Type
import Id (Id)
import Term (Term)
import qualified Term
import Prelude hiding (curry, id, uncurry, (.), (<*>))

pointFree :: PointFree k a -> k '[] a
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k '[] a)

instance Term k => Bound.Bound (PointFree k) where
  -- PointFree f <*> PointFree x = PointFree (f Term.<*> x)

  -- lam n t f = PointFree (Term.curry me)
  --   where
  --     v = Var t n
  --     PointFree body = f (PointFree (mkVar v))
  --     me = case removeVar body v of
  --       Nothing -> Term.const body
  --       Just y -> y

  u64 x = PointFree (to (Term.u64 x))

-- add = PointFree (to Term.add)

instance Term k => Term (Pf k) where
  u64 x = to (Term.u64 x)

  -- add = to Term.add

  tip = me
    where
      me =
        V
          { out = Term.tip,
            removeVar = const Nothing
          }
  const f = me
    where
      me =
        V
          { out = Term.const (out f),
            removeVar = \v -> case removeVar f v of
              Just f' -> Just (Term.swap (Term.const f'))
              _ -> Nothing
          }

-- f <*> x = me
--   where
--     me =
--       V
--         { out = out f Term.<*> out x,
--           removeVar = \v -> case (removeVar f v, removeVar x v) of
--             (Just f', Just x') -> Just (f' Term.<*> x')
--             (_, Just x') -> Just (Term.const f Term.<*> x')
--             (Just f', _) -> Just (f' Term.<*> Term.const x)
--             _ -> Nothing
--         }
-- curry f = me
--   where
--     me =
--       V
--         { out = Term.curry (out f),
--           removeVar = \v -> case removeVar f v of
--             Nothing -> Nothing
--             Just f' -> Just (Term.curry (Term.swap f'))
-- }

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k (v ': env) b)
  }

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

to :: k a b -> Pf k a b
to x = me
  where
    me =
      V
        { out = x,
          removeVar = const Nothing
        }

mkVar :: Term k => Var a -> Pf k '[] a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to Term.tip)
        }
