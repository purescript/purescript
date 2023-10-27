module Language.PureScript.TypeChecker.Utils where

import Prelude

import Data.Text (Text)
import Language.PureScript.TypeClassDictionaries (superclassName)
import Language.PureScript.Types (Constraint(..))

superClassDictionaryNames :: [Constraint a] -> [Text]
superClassDictionaryNames supers =
  [ superclassName pn index
  | (index, Constraint _ pn _ _ _) <- zip [0..] supers
  ]
