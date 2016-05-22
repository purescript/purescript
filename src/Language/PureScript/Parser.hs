-- |
-- A collection of parsers for core data types:
--
--  [@Language.PureScript.Parser.Kinds@] Parser for kinds
--
--  [@Language.PureScript.Parser.Values@] Parser for values
--
--  [@Language.PureScript.Parser.Types@] Parser for types
--
--  [@Language.PureScript.Parser.Declaration@] Parsers for declarations and modules
--
--  [@Language.PureScript.Parser.State@] Parser state, including indentation
--
--  [@Language.PureScript.Parser.Common@] Common parsing utility functions
--
module Language.PureScript.Parser (module P) where

import Language.PureScript.Parser.Common as P
import Language.PureScript.Parser.Declarations as P
import Language.PureScript.Parser.Kinds as P
import Language.PureScript.Parser.Lexer as P
import Language.PureScript.Parser.State as P
import Language.PureScript.Parser.Types as P
