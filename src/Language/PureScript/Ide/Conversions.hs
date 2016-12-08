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
import           Data.Text           (lines, strip, unwords, pack)
import qualified Language.PureScript as P
import           Protolude

properNameT :: Iso' (P.ProperName a) Text
properNameT = iso P.runProperName P.ProperName

identT :: Iso' P.Ident Text
identT = iso P.runIdent P.Ident

prettyTypeT :: P.Type -> Text
prettyTypeT = unwords . map strip . lines . pack . P.prettyPrintType
