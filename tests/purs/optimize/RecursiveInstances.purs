module Main where

import Prelude

import Prim.Row as R
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

class FindKeysAux :: forall k. RL.RowList k -> Constraint
class FindKeysAux a where
  findKeysAux :: Proxy a -> Array String

instance FindKeysAux RL.Nil where
  findKeysAux _ = []

else instance (IsSymbol l, FindKeysAux r) => FindKeysAux (RL.Cons l t r) where
  findKeysAux _ = [ reflectSymbol (Proxy :: Proxy l) ] <> findKeysAux (Proxy :: Proxy r)

findKeys :: forall r rl. RL.RowToList r rl => FindKeysAux rl => Proxy r -> Array String
findKeys _ = findKeysAux (Proxy :: Proxy rl)

findKeys1 = findKeys (Proxy :: Proxy (a :: Unit))
findKeys2 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit))
findKeys3 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit, c :: Unit))
findKeys4 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit, c :: Unit, d :: Unit))
findKeys5 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit, c :: Unit, d :: Unit, e :: Unit))
findKeys6 = findKeys (Proxy :: Proxy (a :: Unit))
findKeys7 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit))
findKeys8 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit, c :: Unit))
findKeys9 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit, c :: Unit, d :: Unit))
findKeys10 = findKeys (Proxy :: Proxy (a :: Unit, b :: Unit, c :: Unit, d :: Unit, e :: Unit))
