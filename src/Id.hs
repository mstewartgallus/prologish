-- |
--
-- Use the trick from functional pearl "On generating unique names by
-- Lennart Augustsson, Mikael Rittri and Dan Synek" to generate a unique
-- stream of ids.
module Id (Id, stream, Stream (..)) where

import Data.Atomics.Counter
import System.IO.Unsafe

data Stream = Stream Id Stream Stream

stream :: IO Stream
stream = do
  counter <- newCounter 0
  stream' counter

stream' :: AtomicCounter -> IO Stream
stream' counter = unsafeInterleaveIO $ do
  u <- unique counter
  x <- stream' counter
  y <- stream' counter
  return (Stream u x y)

unique :: AtomicCounter -> IO Id
unique counter = do
  x <- incrCounter 1 counter
  return (Id (x - 1))

newtype Id = Id Int deriving (Eq, Ord)

instance Show Id where
  show (Id n) = show n
