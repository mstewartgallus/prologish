module Global (Global (..)) where

import Type

data Global a b = Global
  { domain :: ST a,
    codomain :: ST b,
    package :: String,
    name :: String
  }

instance Show (Global a b) where
  show g = package g ++ "/" ++ name g
