{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HasApply (HasExpType (..), HasApply (..)) where

class HasExpType k where
  type Exp k :: k -> k -> k

class HasExpType k => HasApply (t :: k -> *) where
  (<*>) :: t (Exp k a b) -> t a -> t b
