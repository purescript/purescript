-- |
-- Pretty printer for the Javascript AST
--
module Language.PureScript.Pretty.JS
  ( prettyPrintJS
  , prettyPrintJSWithSourceMaps
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
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

-- TODO (Christoph): Get rid of T.unpack / pack

literals :: (Emit gen) => Pattern PrinterState JS gen
literals = mkPattern' match'
  where
  match' :: (Emit gen) => JS -> StateT PrinterState Maybe gen
  match' js = (addMapping' (getSourceSpan js) <>) <$> match js

  match :: (Emit gen) => JS -> StateT PrinterState Maybe gen
  match (JSNumericLiteral _ n) = return $ emit $ T.pack $ either show show n
  match (JSStringLiteral _ s) = return $ emit $ prettyPrintStringJS s
  match (JSBooleanLiteral _ True) = return $ emit "true"
  match (JSBooleanLiteral _ False) = return $ emit "false"
  match (JSArrayLiteral _ xs) = mconcat <$> sequence
    [ return $ emit "[ "
    , intercalate (emit ", ") <$> forM xs prettyPrintJS'
    , return $ emit " ]"
    ]
  match (JSObjectLiteral _ []) = return $ emit "{}"
  match (JSObjectLiteral _ ps) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key <> emit ": ") <>) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate (emit ", \n") $ map (indentString <>) jss
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
    where
    objectPropertyToString :: (Emit gen) => PSString -> gen
    objectPropertyToString s =
      emit $ case decodeString s of
        Just s' | not (identNeedsEscaping s') ->
          s'
        _ ->
          prettyPrintStringJS s
  match (JSBlock _ sts) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (JSVar _ ident) = return $ emit ident
  match (JSVariableIntroduction _ ident value) = mconcat <$> sequence
    [ return $ emit $ "var " <> ident
    , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintJS') value
    ]
  match (JSAssignment _ target value) = mconcat <$> sequence
    [ prettyPrintJS' target
    , return $ emit " = "
    , prettyPrintJS' value
    ]
  match (JSWhile _ cond sts) = mconcat <$> sequence
    [ return $ emit "while ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' sts
    ]
  match (JSFor _ ident start end sts) = mconcat <$> sequence
    [ return $ emit $ "for (var " <> ident <> " = "
    , prettyPrintJS' start
    , return $ emit $ "; " <> ident <> " < "
    , prettyPrintJS' end
    , return $ emit $ "; " <> ident <> "++) "
    , prettyPrintJS' sts
    ]
  match (JSForIn _ ident obj sts) = mconcat <$> sequence
    [ return $ emit $ "for (var " <> ident <> " in "
    , prettyPrintJS' obj
    , return $ emit ") "
    , prettyPrintJS' sts
    ]
  match (JSIfElse _ cond thens elses) = mconcat <$> sequence
    [ return $ emit "if ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintJS') elses
    ]
  match (JSReturn _ value) = mconcat <$> sequence
    [ return $ emit "return "
    , prettyPrintJS' value
    ]
  match (JSThrow _ value) = mconcat <$> sequence
    [ return $ emit "throw "
    , prettyPrintJS' value
    ]
  match (JSBreak _ lbl) = return $ emit $ "break " <> lbl
  match (JSContinue _ lbl) = return $ emit $ "continue " <> lbl
  match (JSLabel _ lbl js) = mconcat <$> sequence
    [ return $ emit $ lbl <> ": "
    , prettyPrintJS' js
    ]
  match (JSComment _ com js) = fmap mconcat $ sequence $
    [ return $ emit "\n"
    , currentIndent
    , return $ emit "/**\n"
    ] ++
    map asLine (concatMap commentLines com) ++
    [ currentIndent
    , return $ emit " */\n"
    , currentIndent
    , prettyPrintJS' js
    ]
    where
    commentLines :: Comment -> [Text]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = T.lines s

    asLine :: (Emit gen) => Text -> StateT PrinterState Maybe gen
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
  match (JSRaw _ js) = return $ emit js
  match _ = mzero

conditional :: Pattern PrinterState JS ((Maybe SourceSpan, JS, JS), JS)
conditional = mkPattern match
  where
  match (JSConditional ss cond th el) = Just ((ss, th, el), cond)
  match _ = Nothing

accessor :: (Emit gen) => Pattern PrinterState JS (gen, JS)
accessor = mkPattern match
  where
  -- WARN: if `prop` does not match the `IdentifierName` grammar, this will generate invalid code; see #2513
  match (JSAccessor _ prop val) = Just (emit prop, val)
  match _ = Nothing

indexer :: (Emit gen) => Pattern PrinterState JS (gen, JS)
indexer = mkPattern' match
  where
  match (JSIndexer _ index val) = (,) <$> prettyPrintJS' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState JS ((Maybe Text, [Text], Maybe SourceSpan), JS)
lam = mkPattern match
  where
  match (JSFunction ss name args ret) = Just ((name, args, ss), ret)
  match _ = Nothing

app :: (Emit gen) => Pattern PrinterState JS (gen, JS)
app = mkPattern' match
  where
  match (JSApp _ val args) = do
    jss <- traverse prettyPrintJS' args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

typeOf :: Pattern PrinterState JS ((), JS)
typeOf = mkPattern match
  where
  match (JSTypeOf _ val) = Just ((), val)
  match _ = Nothing

instanceOf :: Pattern PrinterState JS (JS, JS)
instanceOf = mkPattern match
  where
  match (JSInstanceOf _ val ty) = Just (val, ty)
  match _ = Nothing

unary' :: (Emit gen) => UnaryOperator -> (JS -> Text) -> Operator PrinterState JS gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState JS (gen, JS)
  match = mkPattern match'
    where
    match' (JSUnary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState JS gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState JS gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (JSUnary _ Negate _) = True
  isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState JS gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState JS (JS, JS)
  match = mkPattern match'
    where
    match' (JSBinary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: (Emit gen) => [JS] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) jss

-- |
-- Generate a pretty-printed string representing a Javascript expression
--
prettyPrintJS1 :: (Emit gen) => JS -> gen
prettyPrintJS1 = fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintJS'

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
prettyPrintJSWithSourceMaps :: [JS] -> (Text, [SMap])
prettyPrintJSWithSourceMaps js =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  in (s, mp)

prettyPrintJS :: [JS] -> Text
prettyPrintJS = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements
-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
prettyPrintJS' :: (Emit gen) => JS -> StateT PrinterState Maybe gen
prettyPrintJS' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState JS gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState JS gen
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val <> emit "." <> prop ]
                  , [ Wrap indexer $ \index val -> val <> emit "[" <> index <> emit "]" ]
                  , [ Wrap app $ \args val -> val <> emit "(" <> args <> emit ")" ]
                  , [ unary JSNew "new " ]
                  , [ Wrap lam $ \(name, args, ss) ret -> addMapping' ss <>
                      emit ("function "
                        <> fromMaybe "" name
                        <> "(" <> intercalate ", " args <> ") ")
                        <> ret ]
                  , [ Wrap typeOf $ \_ s -> emit "typeof " <> s ]
                  , [ unary     Not                  "!"
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
                  , [ Wrap conditional $ \(ss, th, el) cond -> cond <> addMapping' ss <> emit " ? " <> prettyPrintJS1 th <> addMapping' ss <> emit " : " <> prettyPrintJS1 el ]
                    ]
