{-# LANGUAGE GADTs #-}

-- | Pretty printer for the JavaScript AST 'Expression (Maybe SourceSpan)
module Language.PureScript.CodeGen.JS.Printer
  ( prettyPrintStatement
  , prettyPrintStatementsAsText
  , prettyPrintStatementsWithSourceMaps
  , prettyPrintExpression
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

-- TODO (Christoph): Get rid of T.unpack / pack

prettyPrintStatement :: Emit gen => AST 'Statement (Maybe SourceSpan) -> StateT PrinterState Maybe gen
prettyPrintStatement (VariableIntroduction _ ident value) = mconcat <$> sequence
  [ return $ emit $ "var " <> ident
  , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintExpression) value
  ]
prettyPrintStatement (Assignment _ target value) = mconcat <$> sequence
  [ prettyPrintExpression target
  , return $ emit " = "
  , prettyPrintExpression value
  ]
prettyPrintStatement (While _ cond sts) = mconcat <$> sequence
  [ return $ emit "while ("
  , prettyPrintExpression cond
  , return $ emit ") "
  , prettyPrintStatement sts
  ]
prettyPrintStatement (For _ ident start end sts) = mconcat <$> sequence
  [ return $ emit $ "for (var " <> ident <> " = "
  , prettyPrintExpression start
  , return $ emit $ "; " <> ident <> " < "
  , prettyPrintExpression end
  , return $ emit $ "; " <> ident <> "++) "
  , prettyPrintStatement sts
  ]
prettyPrintStatement (ForIn _ ident obj sts) = mconcat <$> sequence
  [ return $ emit $ "for (var " <> ident <> " in "
  , prettyPrintExpression obj
  , return $ emit ") "
  , prettyPrintStatement sts
  ]
prettyPrintStatement (IfElse _ cond thens elses) = mconcat <$> sequence
  [ return $ emit "if ("
  , prettyPrintExpression cond
  , return $ emit ") "
  , prettyPrintStatement thens
  , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintStatement) elses
  ]
prettyPrintStatement (Return _ value) = mconcat <$> sequence
  [ return $ emit "return "
  , prettyPrintExpression value
  ]
prettyPrintStatement (ReturnNoResult _) = return $ emit "return"
prettyPrintStatement (Throw _ value) = mconcat <$> sequence
  [ return $ emit "throw "
  , prettyPrintExpression value
  ]
prettyPrintStatement (Block _ sts) = mconcat <$> sequence
  [ return $ emit "{\n"
  , withIndent $ prettyPrintStatements sts
  , return $ emit "\n"
  , currentIndent
  , return $ emit "}"
  ]
prettyPrintStatement (MethodCall _ e) = prettyPrintExpression e
prettyPrintStatement (Comment _ com js) = mconcat <$> sequence
  [ mconcat <$> forM com comment
  , prettyPrintStatement js
  ]

prettyPrintStatements :: Emit gen => [AST 'Statement (Maybe SourceSpan)] -> StateT PrinterState Maybe gen
prettyPrintStatements sts = do
  jss <- forM sts prettyPrintStatement
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) jss

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintStatementsWithSourceMaps :: [AST 'Statement (Maybe SourceSpan)] -> (Text, [SMap])
prettyPrintStatementsWithSourceMaps js =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintStatements) js
  in (s, mp)

prettyPrintStatementsAsText :: [AST 'Statement (Maybe SourceSpan)] -> Text
prettyPrintStatementsAsText = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyPrintStatements

literals :: Emit gen => Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) gen
literals = mkPattern' match' where
  match' :: Emit gen => AST 'Expression (Maybe SourceSpan) -> StateT PrinterState Maybe gen
  match' js = (addMapping' (getSourceSpan js) <>) <$> match js

  match :: Emit gen => AST 'Expression (Maybe SourceSpan) -> StateT PrinterState Maybe gen
  match (NumericLiteral _ n) = return $ emit $ T.pack $ either show show n
  match (StringLiteral _ s) = return $ emit $ prettyPrintStringJS s
  match (BooleanLiteral _ True) = return $ emit "true"
  match (BooleanLiteral _ False) = return $ emit "false"
  match (ArrayLiteral _ xs) = mconcat <$> sequence
    [ return $ emit "[ "
    , intercalate (emit ", ") <$> forM xs prettyPrintExpression
    , return $ emit " ]"
    ]
  match (ObjectLiteral _ []) = return $ emit "{}"
  match (ObjectLiteral _ ps) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key <> emit ": ") <>) . prettyPrintExpression $ value
        indentString <- currentIndent
        return $ intercalate (emit ", \n") $ map (indentString <>) jss
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
    where
    objectPropertyToString :: Emit gen => PSString -> gen
    objectPropertyToString s =
      emit $ case decodeString s of
        Just s' | not (identNeedsEscaping s') ->
          s'
        _ ->
          prettyPrintStringJS s
  match (Var _ ident) = return $ emit ident
  match _ = mzero

comment :: Emit gen => Comment -> StateT PrinterState Maybe gen
comment (LineComment com) = fmap mconcat $ sequence $
  [ return $ emit "\n"
  , currentIndent
  , return $ emit "//" <> emit com <> emit "\n"
  ]
comment (BlockComment com) = fmap mconcat $ sequence $
    [ return $ emit "\n"
    , currentIndent
    , return $ emit "/**\n"
    ] ++
    map asLine (T.lines com) ++
    [ currentIndent
    , return $ emit " */\n"
    , currentIndent
    ]
  where
    asLine :: Emit gen => Text -> StateT PrinterState Maybe gen
    asLine s = do
      i <- currentIndent
      return $ i <> emit " * " <> (emit . removeComments) s <> emit "\n"

    removeComments :: Text -> Text
    removeComments t =
      case T.stripPrefix "*/" t of
        Just rest -> removeComments rest
        Nothing -> case T.uncons t of
          Just (x, xs) -> x `T.cons` removeComments xs
          Nothing -> ""

accessor :: Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) (Text, AST 'Expression (Maybe SourceSpan))
accessor = mkPattern match where
  match :: AST 'Expression (Maybe SourceSpan) -> Maybe (Text, AST 'Expression (Maybe SourceSpan))
  match (Indexer _ (StringLiteral _ prop) val) =
    case decodeString prop of
      Just s | not (identNeedsEscaping s) -> Just (s, val)
      _ -> Nothing
  match _ = Nothing

indexer :: Emit gen => Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) (gen, AST 'Expression (Maybe SourceSpan))
indexer = mkPattern' match where
  match (Indexer _ index val) = (,) <$> prettyPrintExpression index <*> pure val
  match _ = mzero

app :: Emit gen => Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) (gen, AST 'Expression (Maybe SourceSpan))
app = mkPattern' match where
  match (App _ val args) = do
    jss <- traverse prettyPrintExpression args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

instanceOf :: Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) (AST 'Expression (Maybe SourceSpan), AST 'Expression (Maybe SourceSpan))
instanceOf = mkPattern match where
  match :: AST 'Expression (Maybe SourceSpan) -> Maybe (AST 'Expression (Maybe SourceSpan), AST 'Expression (Maybe SourceSpan))
  match (InstanceOf _ val ty) = Just (val, ty)
  match _ = Nothing

unary' :: forall gen. Emit gen => UnaryOperator -> (AST 'Expression (Maybe SourceSpan) -> Text) -> Operator PrinterState (AST 'Expression (Maybe SourceSpan)) gen
unary' op mkStr = Wrap match (<>) where
  match :: Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) (gen, AST 'Expression (Maybe SourceSpan))
  match = mkPattern match' where
    match' (Unary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: Emit gen => UnaryOperator -> Text -> Operator PrinterState (AST 'Expression (Maybe SourceSpan)) gen
unary op str = unary' op (const str)

negateOperator :: Emit gen => Operator PrinterState (AST 'Expression (Maybe SourceSpan)) gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (Unary _ Negate _) = True
  isNegate _ = False

binary :: Emit gen => BinaryOperator -> Text -> Operator PrinterState (AST 'Expression (Maybe SourceSpan)) gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) (AST 'Expression (Maybe SourceSpan), AST 'Expression (Maybe SourceSpan))
  match = mkPattern match'
    where
    match' (Binary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

-- | Generate an indented, pretty-printed string representing a JavaScript expression
prettyPrintExpression :: forall gen. Emit gen => AST 'Expression (Maybe SourceSpan) -> StateT PrinterState Maybe gen
prettyPrintExpression = A.runKleisli $ runPattern matchValue where
  matchValue :: Emit gen => Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) gen
  matchValue =
    buildPrettyPrinter operators2
      (matchLam
        (buildPrettyPrinter operators1 (literals <+> fmap parensPos matchValue)))

  -- This is a bit of a hack. Solving it properly would require changing
  -- the pattern-arrows library to allow stateful functions in the argument to
  -- 'Wrap'. For now, we just break the operator table into two parts.
  matchLam
    :: Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) gen
    -> Pattern PrinterState (AST 'Expression (Maybe SourceSpan)) gen
  matchLam p = mkPattern' match <+> p where
    match :: AST 'Expression (Maybe SourceSpan) -> StateT PrinterState Maybe gen
    match (Function ss name args ret) = do
      retgen <- prettyPrintStatement ret
      pure $ addMapping' ss <>
        emit ("function "
          <> fromMaybe "" name
          <> "(" <> intercalate ", " args <> ") ")
          <> retgen
    match _ = mzero

  operators1 :: OperatorTable PrinterState (AST 'Expression (Maybe SourceSpan)) gen
  operators1 =
    OperatorTable [ [ Wrap indexer $ \index val -> val <> emit "[" <> index <> emit "]" ]
                  , [ Wrap accessor $ \prop val -> val <> emit "." <> emit prop ]
                  , [ Wrap app $ \args val -> val <> emit "(" <> args <> emit ")" ]
                  , [ unary New "new " ]
                  ]

  operators2 :: OperatorTable PrinterState (AST 'Expression (Maybe SourceSpan)) gen
  operators2 =
    OperatorTable [ [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR instanceOf $ \v1 v2 -> v1 <> emit " instanceof " <> v2 ]
                  , [ binary    EqualTo              "==="
                    , binary    NotEqualTo           "!==" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                    ]
