-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Util
-- Description : Generally useful functions and conversions
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Generally useful functions
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.Util
  ( identifierFromIdeDeclaration
  , unwrapMatch
  , unwrapPositioned
  , unwrapPositionedRef
  , completionFromMatch
  , encodeT
  , decodeT
  , discardAnn
  , withEmptyAnn
  , module Language.PureScript.Ide.Conversions
  ) where

import           Protolude                     hiding (decodeUtf8, encodeUtf8)
import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Text.Lazy.Encoding       (decodeUtf8, encodeUtf8)
import qualified Language.PureScript           as P
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Conversions

identifierFromIdeDeclaration :: IdeDeclaration -> Text
identifierFromIdeDeclaration d = case d of
  IdeValue name _ -> runIdentT name
  IdeType name _ -> runProperNameT name
  IdeTypeSynonym name _ -> runProperNameT name
  IdeDataConstructor name _ _ -> runProperNameT name
  IdeTypeClass name -> runProperNameT name
  IdeValueOperator op _ _ _ -> runOpNameT op
  IdeTypeOperator op _ _ _ -> runOpNameT op

discardAnn :: IdeDeclarationAnn -> IdeDeclaration
discardAnn (IdeDeclarationAnn _ d) = d

withEmptyAnn :: IdeDeclaration -> IdeDeclarationAnn
withEmptyAnn = IdeDeclarationAnn emptyAnn

unwrapMatch :: Match a -> a
unwrapMatch (Match (_, ed)) = ed

completionFromMatch :: Match IdeDeclarationAnn -> Completion
completionFromMatch (Match (m, IdeDeclarationAnn ann decl)) =
  Completion {..}
  where
    (complIdentifier, complExpandedType) = case decl of
      IdeValue name type' -> (runIdentT name, prettyTypeT type')
      IdeType name kind -> (runProperNameT name, toS (P.prettyPrintKind kind))
      IdeTypeSynonym name kind -> (runProperNameT name, prettyTypeT kind)
      IdeDataConstructor name _ type' -> (runProperNameT name, prettyTypeT type')
      IdeTypeClass name -> (runProperNameT name, "class")
      IdeValueOperator op ref precedence associativity ->
        (runOpNameT op, showFixity precedence associativity ref op)
      IdeTypeOperator op ref precedence associativity ->
        (runOpNameT op, showFixity precedence associativity ref op)
            
    complModule = runModuleNameT m

    complType = maybe complExpandedType prettyTypeT (annTypeAnnotation ann)
    
    complLocation = annLocation ann

    complDocumentation = Nothing
    
    showFixity p a r o =
      let asso = case a of
            P.Infix -> "infix"
            P.Infixl -> "infixl"
            P.Infixr -> "infixr"
      in T.unwords [asso, show p, r, "as", runOpNameT o]

encodeT :: (ToJSON a) => a -> Text
encodeT = toS . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . toS

unwrapPositioned :: P.Declaration -> P.Declaration
unwrapPositioned (P.PositionedDeclaration _ _ x) = unwrapPositioned x
unwrapPositioned x = x

unwrapPositionedRef :: P.DeclarationRef -> P.DeclarationRef
unwrapPositionedRef (P.PositionedDeclarationRef _ _ x) = unwrapPositionedRef x
unwrapPositionedRef x = x
