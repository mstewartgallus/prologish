{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cbpv.AsView (Action, Data, view) where

import Cbpv
import Control.Category
import Cbpv.Sort
import Prelude hiding ((.), id)

newtype Action (a :: Algebra) = Action { getAction :: String }

newtype Data (a :: Set) = Data { getData :: String }

view :: (Data a -> Data b) -> String
view f = dta f ""

str :: String -> String -> String
str str x = str ++ x

toDta :: (String -> String) -> Data a -> Data b
toDta f (Data x) = Data (f (" ∘ " ++  x))

toAct :: (String -> String) -> Action a -> Action b
toAct f (Action x) = Action (f (" ∘ " ++  x))

act :: (Action a -> Action b) -> String -> String
act f x = case f (Action "id") of
  Action y -> y ++ x

dta :: (Data a -> Data b) -> String -> String
dta f x = case f (Data "id") of
  Data y -> y ++ x

instance Cbpv Action Data where
  to f x = toAct $
    str "(to " . act f . str " " . act x . str ")"

  return f = toAct $
    str "(return " .  dta f . str ")"

  thunk f = toDta $
    str "(thunk " . act f . str ")"
  force f = toAct $
    str "(force " . dta f . str ")"

  unit = toDta $ str "unit"
  f &&& x = toDta $ str "⟨" . dta f . str " , " . dta x . str "⟩"
  first = toDta $ str "π₁"
  second = toDta $  str "π₂"

  absurd = toDta $ str "absurd"
  f ||| x = toDta $ str "[" . dta f . str " , " . dta x . str "]"
  left = toDta $ str "i₁"
  right = toDta $ str "i₂"

  assocOut = toAct $ str "out"
  assocIn = toAct $ str "in"

  curry f = toAct $ str "(λ " . act f . str ")"
  uncurry f = toAct $ str ("(! ") . act f . str ")"

  u64 x = toDta $ str (show x)
  add = toDta $ str "add"
  addLazy = toAct $ str "add"
