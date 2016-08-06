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

import           Protolude
import           Data.Text           (unwords, lines, strip)
import qualified Language.PureScript as P

runProperNameT :: P.ProperName a -> Text
runProperNameT = toS . P.runProperName

runIdentT :: P.Ident -> Text
runIdentT = toS . P.runIdent

runOpNameT :: P.OpName a -> Text
runOpNameT = toS . P.runOpName

runModuleNameT :: P.ModuleName -> Text
runModuleNameT = toS . P.runModuleName

prettyTypeT :: P.Type -> Text
prettyTypeT = unwords . map strip . lines . toS . P.prettyPrintType

