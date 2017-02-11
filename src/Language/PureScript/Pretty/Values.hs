-- |
-- Pretty printer for values
--
module Language.PureScript.Pretty.Values
  ( prettyPrintValue
  , prettyPrintBinder
  , prettyPrintBinderAtom
  ) where

import Prelude.Compat

import Control.Arrow (second)
import Control.Monad (void)

import qualified Data.Monoid as Monoid ((<>))

import qualified Data.Text as T
import Data.Text (Text)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (typeAsBox, typeAtomAsBox, prettyPrintObjectKey)
import Language.PureScript.Types (Constraint(..))
import Language.PureScript.PSString (PSString, prettyPrintString)

import Text.PrettyPrint.Boxes

-- TODO(Christoph): remove T.unpack s

textT :: Text -> Box
textT = text . T.unpack

-- | Render an aligned list of items separated with commas
list :: Char -> Char -> (a -> Box) -> [a] -> Box
list open close _ [] = text [open, close]
list open close f xs = vcat left (zipWith toLine [0 :: Int ..] xs ++ [ text [ close ] ])
  where
  toLine i a = text [ if i == 0 then open else ',', ' ' ] <> f a

ellipsis :: Box
ellipsis = text "..."

prettyPrintObject :: Int -> [(PSString, Maybe (Expr a b))] -> Box
prettyPrintObject d = list '{' '}' prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (PSString, Maybe (Expr a b)) -> Box
  prettyPrintObjectProperty (key, value) = textT (prettyPrintObjectKey key Monoid.<> ": ") <> maybe (text "_") (prettyPrintValue (d - 1)) value

prettyPrintUpdateEntry :: Int -> PSString -> Expr a b -> Box
prettyPrintUpdateEntry d key val = textT (prettyPrintObjectKey key) <> text " = " <> prettyPrintValue (d - 1) val

-- | Pretty-print an expression
prettyPrintValue :: Int -> Expr a b -> Box
prettyPrintValue d _ | d < 0 = text "..."
prettyPrintValue d (IfThenElse cond th el _) =
  (text "if " <> prettyPrintValueAtom (d - 1) cond)
  // moveRight 2 (vcat left [ text "then " <> prettyPrintValueAtom (d - 1) th
                            , text "else " <> prettyPrintValueAtom (d - 1) el
                            ])
prettyPrintValue d (Accessor prop val _) = prettyPrintValueAtom (d - 1) val `before` textT ("." Monoid.<> prettyPrintObjectKey prop)
prettyPrintValue d (ObjectUpdate o ps _) = prettyPrintValueAtom (d - 1) o `beforeWithSpace` list '{' '}' (uncurry (prettyPrintUpdateEntry d)) ps
prettyPrintValue d (ObjectUpdateNested o ps _) = prettyPrintValueAtom (d - 1) o `beforeWithSpace` prettyPrintUpdate ps
  where
    prettyPrintUpdate (PathTree tree) = list '{' '}' printNode (runAssocList tree)
    printNode (key, Leaf val) = prettyPrintUpdateEntry d key val
    printNode (key, Branch val) = textT (prettyPrintObjectKey key) `beforeWithSpace` prettyPrintUpdate val
prettyPrintValue d (App val arg _) = prettyPrintValueAtom (d - 1) val `beforeWithSpace` prettyPrintValueAtom (d - 1) arg
prettyPrintValue d (Abs (Left arg) val _) = text ('\\' : T.unpack (showIdent arg) ++ " -> ") // moveRight 2 (prettyPrintValue (d - 1) val)
prettyPrintValue d (Abs (Right arg) val _) = text ('\\' : T.unpack (prettyPrintBinder arg) ++ " -> ") // moveRight 2 (prettyPrintValue (d - 1) val)
prettyPrintValue d (TypeClassDictionaryConstructorApp className ps _) =
  text (T.unpack (runProperName (disqualify className)) ++ " ") <> prettyPrintValueAtom (d - 1) ps
prettyPrintValue d (Case values binders _) =
  (text "case " <> foldr beforeWithSpace (text "of") (map (prettyPrintValueAtom (d - 1)) values)) //
    moveRight 2 (vcat left (map (prettyPrintCaseAlternative (d - 1)) binders))
prettyPrintValue d (Let ds val _) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds)) //
    (text "in " <> prettyPrintValue (d - 1) val)
prettyPrintValue d (Do els _) =
  text "do " <> vcat left (map (prettyPrintDoNotationElement (d - 1)) els)
prettyPrintValue _ (TypeClassDictionary (Constraint name tys _) _ _ _) = foldl1 beforeWithSpace $ text ("#dict " ++ T.unpack (runProperName (disqualify name))) : map typeAtomAsBox tys
prettyPrintValue _ (DeferredDictionary name _ _) = text $ "#dict " ++ T.unpack (runProperName (disqualify name))
prettyPrintValue _ (TypeClassDictionaryAccessor className ident _) =
    text "#dict-accessor " <> text (T.unpack (runProperName (disqualify className))) <> text "." <> text (T.unpack (showIdent ident)) <> text ">"
prettyPrintValue d (TypedValue _ val _ _) = prettyPrintValue d val
prettyPrintValue d (PositionedValue _ _ val _) = prettyPrintValue d val
prettyPrintValue d (Literal l _) = prettyPrintLiteralValue d l
prettyPrintValue _ (Hole name _) = text "?" <> textT name
prettyPrintValue d expr@AnonymousArgument{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Constructor{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Var{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Op{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@BinaryNoParens{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Parens{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@UnaryMinus{} = prettyPrintValueAtom d expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Int -> Expr a b -> Box
prettyPrintValueAtom d (Literal l _) = prettyPrintLiteralValue d l
prettyPrintValueAtom _ AnonymousArgument{} = text "_"
prettyPrintValueAtom _ (Constructor name _) = text $ T.unpack $ runProperName (disqualify name)
prettyPrintValueAtom _ (Var ident _) = text $ T.unpack $ showIdent (disqualify ident)
prettyPrintValueAtom d (BinaryNoParens op lhs rhs _) =
  prettyPrintValue (d - 1) lhs `beforeWithSpace` printOp op `beforeWithSpace` prettyPrintValue (d - 1) rhs
  where
  printOp (Op (Qualified _ name) _) = text $ T.unpack $ runOpName name
  printOp expr = text "`" <> prettyPrintValue (d - 1) expr `before` text "`"
prettyPrintValueAtom d (TypedValue _ val _ _) = prettyPrintValueAtom d val
prettyPrintValueAtom d (PositionedValue _ _ val _) = prettyPrintValueAtom d val
prettyPrintValueAtom d (Parens expr _) = (text "(" <> prettyPrintValue d expr) `before` text ")"
prettyPrintValueAtom d (UnaryMinus expr _) = text "(-" <> prettyPrintValue d expr <> text ")"
prettyPrintValueAtom d expr = (text "(" <> prettyPrintValue d expr) `before` text ")"

prettyPrintLiteralValue :: Int -> Literal (Expr a b) -> Box
prettyPrintLiteralValue _ (NumericLiteral n) = text $ either show show n
prettyPrintLiteralValue _ (StringLiteral s) = text $ T.unpack $ prettyPrintString s
prettyPrintLiteralValue _ (CharLiteral c) = text $ show c
prettyPrintLiteralValue _ (BooleanLiteral True) = text "true"
prettyPrintLiteralValue _ (BooleanLiteral False) = text "false"
prettyPrintLiteralValue d (ArrayLiteral xs) = list '[' ']' (prettyPrintValue (d - 1)) xs
prettyPrintLiteralValue d (ObjectLiteral ps) = prettyPrintObject (d - 1) $ second Just `map` ps

prettyPrintDeclaration :: Int -> Declaration a b -> Box
prettyPrintDeclaration d _ | d < 0 = ellipsis
prettyPrintDeclaration _ (TypeDeclaration ident ty _) =
  text (T.unpack (showIdent ident) ++ " :: ") <> typeAsBox ty
prettyPrintDeclaration d (ValueDeclaration ident _ [] [GuardedExpr [] val] _) =
  text (T.unpack (showIdent ident) ++ " = ") <> prettyPrintValue (d - 1) val
prettyPrintDeclaration d (BindingGroupDeclaration ds _) =
  vsep 1 left (map (prettyPrintDeclaration (d - 1) . toDecl) ds)
  where
  toDecl (nm, t, e) = ValueDeclaration nm t [] [GuardedExpr [] (void e)] ()
prettyPrintDeclaration d (PositionedDeclaration _ _ decl _) = prettyPrintDeclaration d decl
prettyPrintDeclaration _ _ = internalError "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: Int -> CaseAlternative a b -> Box
prettyPrintCaseAlternative d _ | d < 0 = ellipsis
prettyPrintCaseAlternative d (CaseAlternative binders result) =
  text (T.unpack (T.unwords (map prettyPrintBinderAtom binders))) <> prettyPrintResult result
  where
  prettyPrintResult :: [GuardedExpr a b] -> Box
  prettyPrintResult [GuardedExpr [] v] = text " -> " <> prettyPrintValue (d - 1) v
  prettyPrintResult gs =
    vcat left (map prettyPrintGuardedValue gs)

  prettyPrintGuardedValue :: GuardedExpr a b -> Box
  prettyPrintGuardedValue (GuardedExpr [ConditionGuard grd] val) = foldl1 before
    [ text " | "
    , prettyPrintValue (d - 1) grd
    , text " -> "
    , prettyPrintValue (d - 1) val
    ]
  prettyPrintGuardedValue _ = internalError "There should only be ConditionGuards after desugaring cases"

prettyPrintDoNotationElement :: Int -> DoNotationElement a b -> Box
prettyPrintDoNotationElement d _ | d < 0 = ellipsis
prettyPrintDoNotationElement d (DoNotationValue val _) =
  prettyPrintValue d val
prettyPrintDoNotationElement d (DoNotationBind binder val _) =
  textT (prettyPrintBinder binder Monoid.<> " <- ") <> prettyPrintValue d val
prettyPrintDoNotationElement d (DoNotationLet ds _) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds))
prettyPrintDoNotationElement d (PositionedDoNotationElement _ _ el _) = prettyPrintDoNotationElement d el

prettyPrintBinderAtom :: Binder a b -> Text
prettyPrintBinderAtom NullBinder{} = "_"
prettyPrintBinderAtom (LiteralBinder l _) = prettyPrintLiteralBinder l
prettyPrintBinderAtom (VarBinder ident _) = showIdent ident
prettyPrintBinderAtom (ConstructorBinder ctor [] _) = runProperName (disqualify ctor)
prettyPrintBinderAtom b@ConstructorBinder{} = parensT (prettyPrintBinder b)
prettyPrintBinderAtom (NamedBinder ident binder _) = showIdent ident Monoid.<> "@" Monoid.<> prettyPrintBinder binder
prettyPrintBinderAtom (PositionedBinder _ _ binder _) = prettyPrintBinderAtom binder
prettyPrintBinderAtom (TypedBinder _ binder _) = prettyPrintBinderAtom binder
prettyPrintBinderAtom (OpBinder op _) = runOpName (disqualify op)
prettyPrintBinderAtom (BinaryNoParensBinder op b1 b2 _) =
  prettyPrintBinderAtom b1 Monoid.<> " " Monoid.<> prettyPrintBinderAtom op Monoid.<> " " Monoid.<> prettyPrintBinderAtom b2
prettyPrintBinderAtom (ParensInBinder b _) = parensT (prettyPrintBinder b)

prettyPrintLiteralBinder :: Literal (Binder a b) -> Text
prettyPrintLiteralBinder (StringLiteral str) = prettyPrintString str
prettyPrintLiteralBinder (CharLiteral c) = T.pack (show c)
prettyPrintLiteralBinder (NumericLiteral num) = either (T.pack . show) (T.pack . show) num
prettyPrintLiteralBinder (BooleanLiteral True) = "true"
prettyPrintLiteralBinder (BooleanLiteral False) = "false"
prettyPrintLiteralBinder (ObjectLiteral bs) =
  "{ "
  Monoid.<> T.intercalate ", " (map prettyPrintObjectPropertyBinder bs)
  Monoid.<> " }"
  where
  prettyPrintObjectPropertyBinder :: (PSString, Binder a b) -> Text
  prettyPrintObjectPropertyBinder (key, binder) = prettyPrintObjectKey key Monoid.<> ": " Monoid.<> prettyPrintBinder binder
prettyPrintLiteralBinder (ArrayLiteral bs) =
  "[ "
  Monoid.<> T.intercalate ", " (map prettyPrintBinder bs)
  Monoid.<> " ]"

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder a b -> Text
prettyPrintBinder (ConstructorBinder ctor [] _) = runProperName (disqualify ctor)
prettyPrintBinder (ConstructorBinder ctor args _) = (runProperName (disqualify ctor)) Monoid.<> " " Monoid.<> T.unwords (map prettyPrintBinderAtom args)
prettyPrintBinder (PositionedBinder _ _ binder _) = prettyPrintBinder binder
prettyPrintBinder (TypedBinder _ binder _) = prettyPrintBinder binder
prettyPrintBinder b = prettyPrintBinderAtom b
