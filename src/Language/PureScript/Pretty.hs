-- | A collection of pretty printers for core data types:
--
-- * [@Language.PureScript.Pretty.Kinds@] Pretty printer for kinds
--
-- * [@Language.PureScript.Pretty.Values@] Pretty printer for values
--
-- * [@Language.PureScript.Pretty.Types@] Pretty printer for types
module Language.PureScript.Pretty (module P) where

import Language.PureScript.Pretty.Kinds as P
import Language.PureScript.Pretty.Types as P
import Language.PureScript.Pretty.Values as P
import Language.PureScript.PSString as P (prettyPrintString)
