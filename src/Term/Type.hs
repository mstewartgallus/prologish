{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Term.Type (KnownT, inferT, eqT, ST (..), T, type (~>), type U64) where

import Data.Kind (Type)
import Data.Typeable ((:~:) (..))
import Lambda.Type (KnownT, inferT, eqT, ST (SU64, (:->)), T, type (~>), type U64)

-- fixme... give terms their own very simple type system..
