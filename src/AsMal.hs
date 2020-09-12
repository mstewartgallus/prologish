{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsMal (Expr, asMal, AsObject) where

import Control.Category
import qualified Hoas.Type as Type
import Mal
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type
import Term (Term)
import qualified Term
import Prelude hiding (id, throw, unthrow, (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.+ b) = AsObject a + AsObject b
  AsObject (a Type.* b) = AsObject a * AsObject b
  AsObject Type.Void = Void
  AsObject Type.Unit = Unit
  AsObject Type.B = B
  AsObject Type.U64 = U64

type family AsList a = r | r -> a where
  AsList '[] = Unit
  AsList (h ': t) = AsObject h * AsList t

asMal :: Expr k '[] b -> k Unit (AsObject b)
asMal (E x) = x

newtype Expr k a b = E (k (AsList a) (AsObject b))

instance Mal k => Term (Expr k)

-- absurd = E absurd
-- tip = E left
-- const (E x) = E (right . x)

-- mal (E f) = E (mal f)
-- E f `try` E x = E (f `tryCatch` x)

-- E x `isBoth` (E f, E g) =
--   E $
--     let f' = mal ((right ||| left) . try f)
--         g' = mal ((right ||| left) . try g)
--      in (id ||| x) . try (f' &&& g')

-- E f ||| E g = E (f ||| g)
-- isLeft (E x) = E (x . left)
-- isRight (E x) = E (x . right)

-- isFirst (E x) = E (x . first)
-- isSecond (E x) = E (x . second)
-- E x `isU64` n = E (x . u64 n)

-- pick (E x) = E (x . pick)

-- add = E add
