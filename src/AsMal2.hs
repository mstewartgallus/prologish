{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsMal2 (PointFree, pointFree) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import qualified Hoas.Bound as Bound
import qualified Hoas.Type as Type
import Mal
import Control.Category
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type
import Id (Id)
import Prelude hiding (curry, id, uncurry, (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.+ b) = AsObject a + AsObject b
  AsObject (a Type.|- b) = AsObject a |- AsObject b
  AsObject (a Type.* b) = AsObject a * AsObject b
  AsObject Type.Void = Void
  AsObject Type.Unit = Unit
  AsObject Type.B = B
  AsObject Type.U64 = U64

pointFree :: PointFree k a -> k Unit (AsObject a)
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k Unit (AsObject a))

instance Mal k => Bound.Bound (PointFree k) where
  val (PointFree x) = PointFree (mal right . x)
  jump (PointFree k) (PointFree x) = PointFree (mal (left . x . unit) . k)

  kont n t (PointFree x) f = PointFree (((absurd . me) ||| id) . try id . x)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> body . unit
        Just y -> y . (id &&& unit)

  -- unit = PointFree Term.unit
  -- left (PointFree x) = PointFree (Term.left x)

  -- u64 x = PointFree (lift0 (Term.u64 x))
  -- add (PointFree x) (PointFree y) = PointFree (Term.add x y)

instance Category k => Category (Pf k) where
  id = lift0 id

instance Mal k => HasSum (Pf k) where
instance Mal k => HasProduct (Pf k) where
instance Mal k => HasCoexp (Pf k) where
instance Mal k => Mal (Pf k) where
--   tip = me
--     where
--       me =
--         V
--           { out = Term.tip,
--             removeVar = const Nothing
--           }
--   const f = me
--     where
--       me =
--         V
--           { out = Term.const (out f),
--             removeVar = \v -> case removeVar f v of
--               Just f' -> Just (Term.swap (Term.const f'))
--               _ -> Nothing
--           }
--   swap x = me
--     where
--       me =
--         V
--           { out = Term.swap (out x),
--             removeVar = \v -> case removeVar x v of
--               -- Just x' -> Just (Term.const x')
--               _ -> error "todo"
--           }

--   mal x y = me
--     where
--       me =
--         V
--           { out = Term.mal (out x) (out y),
--             removeVar = \v -> case (removeVar x v, removeVar y v) of
--               (Just x', Just y') -> Just $ Term.mal (Term.swap x') y'
--               (_, Just y') -> Just $ Term.mal (Term.swap (Term.const x)) y'
--               (Just x', _) -> Just $ Term.mal (Term.swap x') (Term.const y)
--               _ -> Nothing
--           }

--   try x y z = me
--     where
--       me =
--         V
--           { out = Term.try (out x) (out y) (out z),
--             removeVar = \v -> case (removeVar x v, removeVar y v, removeVar z v) of
--               -- (Just x', Just y', Just z') -> Just $ Term.try x' y' z'
--               -- (_, Just y') -> Just $ Term.mal (Term.swap (Term.const x)) y'
--               -- (Just x', _) -> Just $ Term.mal (Term.swap x') (Term.const y)
--               _ -> error "todo"
--           }

--   unit = lift0 Term.unit

--   absurd = lift1 Term.absurd
--   left = lift1 Term.left
--   right = lift1 Term.right

--   u64 x = lift0 (Term.u64 x)
--   add = lift2 Term.add

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k ((AsObject v) * env) b)
  }

data Var a = Var (Type.ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = Type.eqT t t'
  | otherwise = Nothing

mkVar :: Mal k => Var a -> Pf k Unit (AsObject a)
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 first)
        }

lift0 :: k a b -> Pf k a b
lift0 x = me
  where
    me =
      V
        { out = x,
          removeVar = const Nothing
        }

lift1 ::
  Mal k =>
  (forall env. k env a -> k env c) ->
  Pf k env a ->
  Pf k env c
lift1 f x = me
  where
    me =
      V
        { out = f (out x),
          removeVar = \v -> case removeVar x v of
            Just x' -> Just $ lift1 f x'
            _ -> Nothing
        }

lift2 ::
  Mal k =>
  (forall env. k env a -> k env b -> k env c) ->
  Pf k env a ->
  Pf k env b ->
  Pf k env c
lift2 f x y = me
  where
    me =
      V
        { out = f (out x) (out y),
          removeVar = \v -> case (removeVar x v, removeVar y v) of
            (Just x', Just y') -> Just $ lift2 f x' y'
            -- (_, Just y') -> Just $ lift2 f (Term.const x) y'
            -- (Just x', _) -> Just $ lift2 f x' (Term.const y)
            -- _ -> Nothing
        }

