module Language.JavaScript.AST.JSCommaList where

import Prelude
import Language.JavaScript.Parser.AST (JSCommaList(JSLNil, JSLOne, JSLCons), JSAnnot(JSNoAnnot))

fromCommaList :: JSCommaList a -> [a]
fromCommaList JSLNil = []
fromCommaList (JSLOne x) = [x]
fromCommaList (JSLCons l _ x) = fromCommaList l ++ [x]

-- comma lists are reverse-consed
toCommaList :: [a] -> JSCommaList a
toCommaList [] = JSLNil
toCommaList [x] = JSLOne x
toCommaList l = go $ reverse l
  where
    go [x] = JSLOne x
    go (h:t)= JSLCons (go t) JSNoAnnot h
    go [] = error "Invalid case in comma-list"
