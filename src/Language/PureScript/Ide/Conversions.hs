-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Conversions
-- Description : Conversions to Text for PureScript types
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Conversions to Text for PureScript types
-----------------------------------------------------------------------------

module Language.PureScript.Ide.Conversions where

import           Prelude.Compat
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Language.PureScript as P

runProperNameT :: P.ProperName a -> Text
runProperNameT = T.pack . P.runProperName

runIdentT :: P.Ident -> Text
runIdentT = T.pack . P.runIdent

runOpNameT :: P.OpName a -> Text
runOpNameT = T.pack . P.runOpName

runModuleNameT :: P.ModuleName -> Text
runModuleNameT = T.pack . P.runModuleName

prettyTypeT :: P.Type -> Text
prettyTypeT = T.unwords . fmap T.strip . T.lines . T.pack . P.prettyPrintType

