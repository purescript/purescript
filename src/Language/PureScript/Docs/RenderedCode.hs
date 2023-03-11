
-- | Data types and functions for representing a simplified form of PureScript
-- code, intended for use in e.g. HTML documentation.

module Language.PureScript.Docs.RenderedCode (module RenderedCode) where

import Language.PureScript.Docs.RenderedCode.Types as RenderedCode
    ( alias,
      aliasName,
      asContainingModule,
      dataCtor,
      fromQualified,
      ident,
      keyword,
      keywordAs,
      keywordClass,
      keywordData,
      keywordFixity,
      keywordForall,
      keywordType,
      keywordWhere,
      maybeToContainingModule,
      outputWith,
      roleAnn,
      sp,
      syntax,
      typeCtor,
      typeOp,
      typeVar,
      ContainingModule(..),
      FixityAlias,
      Link(..),
      Namespace(..),
      RenderedCode,
      RenderedCodeElement(..) )
import Language.PureScript.Docs.RenderedCode.RenderType as RenderedCode
    ( renderRow,
      renderType,
      renderType',
      renderTypeAtom,
      renderTypeAtom',
      renderTypeWithRole )
