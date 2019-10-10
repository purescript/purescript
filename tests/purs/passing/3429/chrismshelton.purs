-- Example submitted by chrismshelton.
-- Cf. failing/3429-chrismshelton.purs
module ChrisMShelton where

import Prelude

class CA v a where
  lengthA :: forall m . v m a -> Int

class (CA w a) <= CB v w a | v a -> w, w a -> v where
  lengthB :: v a -> Int
  fromA :: forall m . w m a -> v a
  toA   :: forall m . v a -> w m a

defaultLengthB :: forall v w a . (CB v w a) => v a -> Int
defaultLengthB v = lengthA $ toA v

data DA m a = DA Int

data DB a = DB Int

instance caDA :: CA DA a where
  lengthA (DA i) = i

instance cbDB :: (CA DA a) => CB DB DA a where
  lengthB x = defaultLengthB x
  fromA (DA a) = (DB a)
  toA (DB a) = (DA a)
