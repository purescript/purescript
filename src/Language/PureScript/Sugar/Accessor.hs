-- |
module Language.PureScript.Sugar.Accessor
  ( desugarAccessorModule
  ) where

import           Prelude

import           Control.Monad.Writer

import           Data.Monoid (Any(..))
import           Language.PureScript.AST
import Language.PureScript.Constants.Libs qualified as C
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           Language.PureScript.Types

-- | Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
desugarAccessorModule :: [ExternsFile] -> Module -> Module
desugarAccessorModule externs m
  | not (any (\e -> efModuleName e == ModuleName "Data.Record") externs) = m
desugarAccessorModule _externs (Module ss coms mn ds exts) =
  let (ds', Any used) = runWriter $ traverse desugarAccessor ds
      extraImports    = if used
        then addDefaultImport (Qualified (ByModuleName C.M_Data_Record) C.M_Data_Record)
          . addDefaultImport (Qualified (ByModuleName C.M_Type_Proxy) C.M_Type_Proxy)
        else id
  in  extraImports $ Module ss coms mn ds' exts

-- | Desugar a single let expression
desugarAccessor :: Declaration -> Writer Any Declaration
desugarAccessor decl =
  let (f, _, _) = everywhereOnValuesM pure replace pure in f decl
 where
  replace :: Expr -> Writer Any Expr
  replace (Accessor label e) = do
    tell (Any True)
    pure $ App
      (App
        (Var nullSourceSpan C.I_getField)
        (TypedValue
          False
          (Constructor nullSourceSpan C.C_Proxy)
          (TypeApp nullSourceAnn
                   (TypeConstructor nullSourceAnn C.Proxy)
                   (TypeLevelString nullSourceAnn label)
          )
        )
      )
      e
  replace other = pure other
