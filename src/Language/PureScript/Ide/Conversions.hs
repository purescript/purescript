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

import           Control.Lens.Iso
import           Data.Text           (lines, strip, unwords)
import qualified Language.PureScript as P
import           Protolude

runProperNameT :: P.ProperName a -> Text
runProperNameT = toS . P.runProperName

properNameT :: Iso' (P.ProperName a) Text
properNameT = iso (toS . P.runProperName) (P.ProperName . toS)

runIdentT :: P.Ident -> Text
runIdentT = toS . P.runIdent

identT :: Iso' P.Ident Text
identT = iso (toS . P.runIdent) (P.Ident . toS)

runOpNameT :: P.OpName a -> Text
runOpNameT = toS . P.runOpName

runModuleNameT :: P.ModuleName -> Text
runModuleNameT = toS . P.runModuleName

prettyTypeT :: P.Type -> Text
prettyTypeT = unwords . map strip . lines . toS . P.prettyPrintType
