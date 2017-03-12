-- | Pretty printer for the JavaScript AST
module Language.PureScript.CodeGen.JS.Printer
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
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

-- TODO (Christoph): Get rid of T.unpack / pack

literals :: (Emit gen) => Pattern PrinterState AST gen
literals = mkPattern' match'
  where
  match' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match' js = (addMapping' (getSourceSpan js) <>) <$> match js

  match :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match (NumericLiteral _ n) = return $ emit $ T.pack $ either show show n
  match (StringLiteral _ s) = return $ emit $ prettyPrintStringJS s
  match (BooleanLiteral _ True) = return $ emit "true"
  match (BooleanLiteral _ False) = return $ emit "false"
  match (ArrayLiteral _ xs) = mconcat <$> sequence
    [ return $ emit "[ "
    , intercalate (emit ", ") <$> forM xs prettyPrintJS'
    , return $ emit " ]"
    ]
  match (ObjectLiteral _ []) = return $ emit "{}"
  match (ObjectLiteral _ ps) = mconcat <$> sequence
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
  match (Block _ sts) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (Var _ ident) = return $ emit ident
  match (VariableIntroduction _ ident value) = mconcat <$> sequence
    [ return $ emit $ "var " <> ident
    , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintJS') value
    ]
  match (Assignment _ target value) = mconcat <$> sequence
    [ prettyPrintJS' target
    , return $ emit " = "
    , prettyPrintJS' value
    ]
  match (While _ cond sts) = mconcat <$> sequence
    [ return $ emit "while ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' sts
    ]
  match (For _ ident start end sts) = mconcat <$> sequence
    [ return $ emit $ "for (var " <> ident <> " = "
    , prettyPrintJS' start
    , return $ emit $ "; " <> ident <> " < "
    , prettyPrintJS' end
    , return $ emit $ "; " <> ident <> "++) "
    , prettyPrintJS' sts
    ]
  match (ForIn _ ident obj sts) = mconcat <$> sequence
    [ return $ emit $ "for (var " <> ident <> " in "
    , prettyPrintJS' obj
    , return $ emit ") "
    , prettyPrintJS' sts
    ]
  match (IfElse _ cond thens elses) = mconcat <$> sequence
    [ return $ emit "if ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintJS') elses
    ]
  match (Return _ value) = mconcat <$> sequence
    [ return $ emit "return "
    , prettyPrintJS' value
    ]
  match (ReturnNoResult _) = return $ emit "return"
  match (Throw _ value) = mconcat <$> sequence
    [ return $ emit "throw "
    , prettyPrintJS' value
    ]
  match (Comment _ com js) = mconcat <$> sequence
    [ mconcat <$> forM com comment
    , prettyPrintJS' js
    ]
  match _ = mzero

  comment :: (Emit gen) => Comment -> StateT PrinterState Maybe gen
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

accessor :: Pattern PrinterState AST (Text, AST)
accessor = mkPattern match
  where
  match (Indexer _ (StringLiteral _ prop) val) =
    case decodeString prop of
      Just s | not (identNeedsEscaping s) -> Just (s, val)
      _ -> Nothing
  match _ = Nothing

indexer :: (Emit gen) => Pattern PrinterState AST (gen, AST)
indexer = mkPattern' match
  where
  match (Indexer _ index val) = (,) <$> prettyPrintJS' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState AST ((Maybe Text, [Text], Maybe SourceSpan), AST)
lam = mkPattern match
  where
  match (Function ss name args ret) = Just ((name, args, ss), ret)
  match _ = Nothing

app :: (Emit gen) => Pattern PrinterState AST (gen, AST)
app = mkPattern' match
  where
  match (App _ val args) = do
    jss <- traverse prettyPrintJS' args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

instanceOf :: Pattern PrinterState AST (AST, AST)
instanceOf = mkPattern match
  where
  match (InstanceOf _ val ty) = Just (val, ty)
  match _ = Nothing

unary' :: (Emit gen) => UnaryOperator -> (AST -> Text) -> Operator PrinterState AST gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState AST (gen, AST)
  match = mkPattern match'
    where
    match' (Unary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState AST gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState AST gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (Unary _ Negate _) = True
  isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState AST gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState AST (AST, AST)
  match = mkPattern match'
    where
    match' (Binary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: (Emit gen) => [AST] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) jss

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintJSWithSourceMaps :: [AST] -> (Text, [SMap])
prettyPrintJSWithSourceMaps js =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  in (s, mp)

prettyPrintJS :: [AST] -> Text
prettyPrintJS = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

-- | Generate an indented, pretty-printed string representing a JavaScript expression
prettyPrintJS' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
prettyPrintJS' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState AST gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState AST gen
  operators =
    OperatorTable [ [ Wrap indexer $ \index val -> val <> emit "[" <> index <> emit "]" ]
                  , [ Wrap accessor $ \prop val -> val <> emit "." <> emit prop ]
                  , [ Wrap app $ \args val -> val <> emit "(" <> args <> emit ")" ]
                  , [ unary New "new " ]
                  , [ Wrap lam $ \(name, args, ss) ret -> addMapping' ss <>
                      emit ("function "
                        <> fromMaybe "" name
                        <> "(" <> intercalate ", " args <> ") ")
                        <> ret ]
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
                    ]
