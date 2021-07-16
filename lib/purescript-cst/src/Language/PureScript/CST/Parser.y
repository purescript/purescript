{
module Language.PureScript.CST.Parser
  ( parseType
  , parseExpr
  , parseDecl
  , parseIdent
  , parseOperator
  , parseModule
  , parseImportDeclP
  , parseDeclP
  , parseExprP
  , parseTypeP
  , parseModuleNameP
  , parseQualIdentP
  , parse
  , PartialResult(..)
  ) where

import Prelude hiding (lex)

import Control.Monad ((<=<), when)
import Data.Foldable (foldl', for_, toList)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Traversable (for, sequence)
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Flatten (flattenType)
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types
import Language.PureScript.CST.Utils
import qualified Language.PureScript.Names as N
import qualified Language.PureScript.Roles as R
import Language.PureScript.PSString (PSString)
}

%expect 0

%name parseType type
%name parseExpr expr
%name parseIdent ident
%name parseOperator op
%name parseModuleBody moduleBody
%name parseDecl decl
%partial parseImportDeclP importDeclP
%partial parseDeclP declP
%partial parseExprP exprP
%partial parseTypeP typeP
%partial parseModuleNameP moduleNameP
%partial parseQualIdentP qualIdentP
%partial parseModuleHeader moduleHeader
%partial parseDoStatement doStatement
%partial parseDoExpr doExpr
%partial parseDoNext doNext
%partial parseGuardExpr guardExpr
%partial parseGuardNext guardNext
%partial parseGuardStatement guardStatement
%partial parseClassSignature classSignature
%partial parseClassSuper classSuper
%partial parseClassNameAndFundeps classNameAndFundeps
%partial parseBinderAndArrow binderAndArrow
%tokentype { SourceToken }
%monad { Parser }
%error { parseError }
%lexer { lexer } { SourceToken _ TokEof }

%token
  '('                { SourceToken _ TokLeftParen }
  ')'                { SourceToken _ TokRightParen }
  '{'                { SourceToken _ TokLeftBrace }
  '}'                { SourceToken _ TokRightBrace }
  '['                { SourceToken _ TokLeftSquare }
  ']'                { SourceToken _ TokRightSquare }
  '\{'               { SourceToken _ TokLayoutStart }
  '\}'               { SourceToken _ TokLayoutEnd }
  '\;'               { SourceToken _ TokLayoutSep }
  '<-'               { SourceToken _ (TokLeftArrow _) }
  '->'               { SourceToken _ (TokRightArrow _) }
  '<='               { SourceToken _ (TokOperator [] sym) | isLeftFatArrow sym }
  '=>'               { SourceToken _ (TokRightFatArrow _) }
  ':'                { SourceToken _ (TokOperator [] ":") }
  '::'               { SourceToken _ (TokDoubleColon _) }
  '='                { SourceToken _ TokEquals }
  '|'                { SourceToken _ TokPipe }
  '`'                { SourceToken _ TokTick }
  '.'                { SourceToken _ TokDot }
  ','                { SourceToken _ TokComma }
  '_'                { SourceToken _ TokUnderscore }
  '\\'               { SourceToken _ TokBackslash }
  '-'                { SourceToken _ (TokOperator [] "-") }
  '@'                { SourceToken _ (TokOperator [] "@") }
  '#'                { SourceToken _ (TokOperator [] "#") }
  'ado'              { SourceToken _ (TokLowerName _ "ado") }
  'as'               { SourceToken _ (TokLowerName [] "as") }
  'case'             { SourceToken _ (TokLowerName [] "case") }
  'class'            { SourceToken _ (TokLowerName [] "class") }
  'data'             { SourceToken _ (TokLowerName [] "data") }
  'derive'           { SourceToken _ (TokLowerName [] "derive") }
  'do'               { SourceToken _ (TokLowerName _ "do") }
  'else'             { SourceToken _ (TokLowerName [] "else") }
  'false'            { SourceToken _ (TokLowerName [] "false") }
  'forall'           { SourceToken _ (TokForall ASCII) }
  'forallu'          { SourceToken _ (TokForall Unicode) }
  'foreign'          { SourceToken _ (TokLowerName [] "foreign") }
  'hiding'           { SourceToken _ (TokLowerName [] "hiding") }
  'import'           { SourceToken _ (TokLowerName [] "import") }
  'if'               { SourceToken _ (TokLowerName [] "if") }
  'in'               { SourceToken _ (TokLowerName [] "in") }
  'infix'            { SourceToken _ (TokLowerName [] "infix") }
  'infixl'           { SourceToken _ (TokLowerName [] "infixl") }
  'infixr'           { SourceToken _ (TokLowerName [] "infixr") }
  'instance'         { SourceToken _ (TokLowerName [] "instance") }
  'kind'             { SourceToken _ (TokLowerName [] "kind") }
  'let'              { SourceToken _ (TokLowerName [] "let") }
  'module'           { SourceToken _ (TokLowerName [] "module") }
  'newtype'          { SourceToken _ (TokLowerName [] "newtype") }
  'nominal'          { SourceToken _ (TokLowerName [] "nominal") }
  'phantom'          { SourceToken _ (TokLowerName [] "phantom") }
  'of'               { SourceToken _ (TokLowerName [] "of") }
  'representational' { SourceToken _ (TokLowerName [] "representational") }
  'role'             { SourceToken _ (TokLowerName [] "role") }
  'then'             { SourceToken _ (TokLowerName [] "then") }
  'true'             { SourceToken _ (TokLowerName [] "true") }
  'type'             { SourceToken _ (TokLowerName [] "type") }
  'where'            { SourceToken _ (TokLowerName [] "where") }
  '(->)'             { SourceToken _ (TokSymbolArr _) }
  '(..)'             { SourceToken _ (TokSymbolName [] "..") }
  LOWER              { SourceToken _ (TokLowerName [] _) }
  QUAL_LOWER         { SourceToken _ (TokLowerName _ _) }
  UPPER              { SourceToken _ (TokUpperName [] _) }
  QUAL_UPPER         { SourceToken _ (TokUpperName _ _) }
  SYMBOL             { SourceToken _ (TokSymbolName [] _) }
  QUAL_SYMBOL        { SourceToken _ (TokSymbolName _ _) }
  OPERATOR           { SourceToken _ (TokOperator [] _) }
  QUAL_OPERATOR      { SourceToken _ (TokOperator _ _) }
  LIT_HOLE           { SourceToken _ (TokHole _) }
  LIT_CHAR           { SourceToken _ (TokChar _ _) }
  LIT_STRING         { SourceToken _ (TokString _ _) }
  LIT_RAW_STRING     { SourceToken _ (TokRawString _) }
  LIT_INT            { SourceToken _ (TokInt _ _) }
  LIT_NUMBER         { SourceToken _ (TokNumber _ _) }

%%

many(a) :: { NE.NonEmpty a }
  : many1(a) %shift { NE.reverse $1 }

many1(a) :: { NE.NonEmpty a }
  : a { pure $1 }
  | many1(a) a { NE.cons $2 $1 }

manySep(a, sep) :: { NE.NonEmpty a }
  : manySep1(a, sep) { NE.reverse $1 }

manySep1(a, sep) :: { NE.NonEmpty a }
  : a { pure $1 }
  | manySep1(a, sep) sep a { NE.cons $3 $1 }

manySepOrEmpty(a, sep) :: { [a] }
  : {- empty -} { [] }
  | manySep(a, sep) { NE.toList $1 }

manyOrEmpty(a) :: { [a] }
  : {- empty -} { [] }
  | many(a) { NE.toList $1 }

sep(a, s) :: { Separated a }
  : sep1(a, s) { separated $1 }

sep1(a, s) :: { [(SourceToken, a)] }
  : a %shift { [(placeholder, $1)] }
  | sep1(a, s) s a { ($2, $3) : $1 }

delim(a, b, c, d) :: { Delimited b }
  : a d { Wrapped $1 Nothing $2 }
  | a sep(b, c) d { Wrapped $1 (Just $2) $3 }

moduleName :: { Name N.ModuleName }
  : UPPER {% upperToModuleName $1 }
  | QUAL_UPPER {% upperToModuleName $1 }

qualProperName :: { QualifiedProperName }
  : UPPER {% qualifiedProperName <\$> toQualifiedName N.ProperName $1 }
  | QUAL_UPPER {% qualifiedProperName <\$> toQualifiedName N.ProperName $1 }

properName :: { ProperName }
  : UPPER {% properName <\$> toName N.ProperName $1 }

qualIdent :: { QualifiedName Ident }
  : LOWER {% toQualifiedName Ident $1 }
  | QUAL_LOWER {% toQualifiedName Ident $1 }
  | 'as' {% toQualifiedName Ident $1 }
  | 'hiding' {% toQualifiedName Ident $1 }
  | 'kind' {% toQualifiedName Ident $1 }
  | 'role' {% toQualifiedName Ident $1 }
  | 'nominal' {% toQualifiedName Ident $1 }
  | 'representational' {% toQualifiedName Ident $1 }
  | 'phantom' {% toQualifiedName Ident $1 }

ident :: { Name Ident }
  : LOWER {% toName Ident $1 }
  | 'as' {% toName Ident $1 }
  | 'hiding' {% toName Ident $1 }
  | 'kind' {% toName Ident $1 }
  | 'role' {% toName Ident $1 }
  | 'nominal' {% toName Ident $1 }
  | 'representational' {% toName Ident $1 }
  | 'phantom' {% toName Ident $1 }

qualOp :: { QualifiedOpName }
  : OPERATOR {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | QUAL_OPERATOR {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | '<=' {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | '-' {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | '#' {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | ':' {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }

op :: { OpName }
  : OPERATOR {% opName <\$> toName N.OpName $1 }
  | '<=' {% opName <\$> toName N.OpName $1 }
  | '-' {% opName <\$> toName N.OpName $1 }
  | '#' {% opName <\$> toName N.OpName $1 }
  | ':' {% opName <\$> toName N.OpName $1 }

qualSymbol :: { QualifiedOpName }
  : SYMBOL {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | QUAL_SYMBOL {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }
  | '(..)' {% qualifiedOpName <\$> toQualifiedName N.OpName $1 }

symbol :: { OpName }
  : SYMBOL {% opName <\$> toName N.OpName $1 }
  | '(..)' {% opName <\$> toName N.OpName $1 }

label :: { Label }
  : LOWER { toLabel $1 }
  | LIT_STRING { toLabel $1 }
  | LIT_RAW_STRING { toLabel $1 }
  | 'ado' { toLabel $1 }
  | 'as' { toLabel $1 }
  | 'case' { toLabel $1 }
  | 'class' { toLabel $1 }
  | 'data' { toLabel $1 }
  | 'derive' { toLabel $1 }
  | 'do' { toLabel $1 }
  | 'else' { toLabel $1 }
  | 'false' { toLabel $1 }
  | 'forall' { toLabel $1 }
  | 'foreign' { toLabel $1 }
  | 'hiding' { toLabel $1 }
  | 'import' { toLabel $1 }
  | 'if' { toLabel $1 }
  | 'in' { toLabel $1 }
  | 'infix' { toLabel $1 }
  | 'infixl' { toLabel $1 }
  | 'infixr' { toLabel $1 }
  | 'instance' { toLabel $1 }
  | 'kind' { toLabel $1 }
  | 'let' { toLabel $1 }
  | 'module' { toLabel $1 }
  | 'newtype' { toLabel $1 }
  | 'nominal' { toLabel $1 }
  | 'of' { toLabel $1 }
  | 'phantom' { toLabel $1 }
  | 'representational' { toLabel $1 }
  | 'role' { toLabel $1 }
  | 'then' { toLabel $1 }
  | 'true' { toLabel $1 }
  | 'type' { toLabel $1 }
  | 'where' { toLabel $1 }

hole :: { Name Ident }
  : LIT_HOLE {% toName Ident $1 }

string :: { (SourceToken, PSString) }
  : LIT_STRING { toString $1 }
  | LIT_RAW_STRING { toString $1 }

char :: { (SourceToken, Char) }
  : LIT_CHAR { toChar $1 }

number :: { (SourceToken, Either Integer Double) }
  : LIT_INT { toNumber $1 }
  | LIT_NUMBER { toNumber $1 }

int :: { (SourceToken, Integer) }
  : LIT_INT { toInt $1 }

boolean :: { (SourceToken, Bool) }
  : 'true' { toBoolean $1 }
  | 'false' { toBoolean $1 }

type :: { Type () }
  : type1 %shift { $1 }
  | type1 '::' type { TypeKinded () $1 $2 $3 }

type1 :: { Type () }
  : type2 { $1 }
  | forall many(typeVarBinding) '.' type1 { TypeForall () $1 $2 $3 $4 }

type2 :: { Type () }
  : type3 %shift { $1 }
  | type3 '->' type1 { TypeArr () $1 $2 $3 }
  | type3 '=>' type1 {% do cs <- toConstraint $1; pure $ TypeConstrained () cs $2 $3 }

type3 :: { Type () }
  : type4 { $1 }
  | type3 qualOp type4 { TypeOp () $1 (getQualifiedOpName $2) $3 }

type4 :: { Type () }
  : type5 %shift { $1 }
  | '#' type4 {% addWarning ($1 : toList (flattenType $2)) WarnDeprecatedRowSyntax *> pure (TypeUnaryRow () $1 $2) }

type5 :: { Type () }
  : typeAtom { $1 }
  | type5 typeAtom { TypeApp () $1 $2 }

typeAtom :: { Type ()}
  : '_' { TypeWildcard () $1 }
  | ident { TypeVar () $1 }
  | qualProperName { TypeConstructor () (getQualifiedProperName $1) }
  | qualSymbol { TypeOpName () (getQualifiedOpName $1) }
  | string { uncurry (TypeString ()) $1 }
  | hole { TypeHole () $1 }
  | '(->)' { TypeArrName () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' type1 ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' type ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

-- Due to a conflict between row syntax and kinded type syntax, we require
-- kinded type variables to be wrapped in parens. Thus `(a :: Foo)` is always a
-- row, and to annotate `a` with kind `Foo`, one must use `((a) :: Foo)`.
typeKindedAtom :: { Type () }
  : '_' { TypeWildcard () $1 }
  | qualProperName { TypeConstructor () (getQualifiedProperName $1) }
  | qualSymbol { TypeOpName () (getQualifiedOpName $1) }
  | hole { TypeHole () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' type1 ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' type ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

row :: { Row () }
  : {- empty -} { Row Nothing Nothing }
  | '|' type { Row Nothing (Just ($1, $2)) }
  | sep(rowLabel, ',') { Row (Just $1) Nothing }
  | sep(rowLabel, ',') '|' type { Row (Just $1) (Just ($2, $3)) }

rowLabel :: { Labeled Label (Type ()) }
  : label '::' type { Labeled $1 $2 $3 }

typeVarBinding :: { TypeVarBinding () }
  : ident { TypeVarName $1 }
  | '(' ident '::' type ')' {% checkNoWildcards $4 *> pure (TypeVarKinded (Wrapped $1 (Labeled $2 $3 $4) $5)) }

forall :: { SourceToken }
  : 'forall' { $1 }
  | 'forallu' { $1 }

exprWhere :: { Where () }
  : expr %shift { Where $1 Nothing }
  | expr 'where' '\{' manySep(letBinding, '\;') '\}' { Where $1 (Just ($2, $4)) }

expr :: { Expr () }
  : expr1 %shift { $1 }
  | expr1 '::' type { ExprTyped () $1 $2 $3 }

expr1 :: { Expr () }
  : expr2 %shift { $1 }
  | expr1 qualOp expr2 %shift { ExprOp () $1 (getQualifiedOpName $2) $3 }

expr2 :: { Expr () }
  : expr3 { $1 }
  | expr2 '`' exprBacktick '`' expr3 { ExprInfix () $1 (Wrapped $2 $3 $4) $5 }

exprBacktick :: { Expr () }
  : expr3 { $1 }
  | exprBacktick qualOp expr3 { ExprOp () $1 (getQualifiedOpName $2) $3 }

expr3 :: { Expr () }
  : expr4 %shift { $1 }
  | '-' expr3 { ExprNegate () $1 $2 }

expr4 :: { Expr () }
  : expr5 { $1 }
  | expr4 expr5
      { -- Record application/updates can introduce a function application
        -- associated to the right, so we need to correct it.
        case $2 of
          ExprApp _ lhs rhs ->
            ExprApp () (ExprApp () $1 lhs) rhs
          _ -> ExprApp () $1 $2
      }

expr5 :: { Expr () }
  : expr6 { $1 }
  | 'if' expr 'then' expr 'else' expr { ExprIf () (IfThenElse $1 $2 $3 $4 $5 $6) }
  | doBlock { ExprDo () $1 }
  | adoBlock 'in' expr { ExprAdo () $ uncurry AdoBlock $1 $2 $3 }
  | '\\' many(binderAtom) '->' expr { ExprLambda () (Lambda $1 $2 $3 $4) }
  | 'let' '\{' manySep(letBinding, '\;') '\}' 'in' expr { ExprLet () (LetIn $1 $3 $5 $6) }
  | 'case' sep(expr, ',') 'of' '\{' manySep(caseBranch, '\;') '\}' { ExprCase () (CaseOf $1 $2 $3 $5) }
  -- These special cases handle some idiosynchratic syntax that the current
  -- parser allows. Technically the parser allows the rhs of a case branch to be
  -- at any level, but this is ambiguous. We allow it in the case of a singleton
  -- case, since this is used in the wild.
  | 'case' sep(expr, ',') 'of' '\{' sep(binder1, ',') '->' '\}' exprWhere
      { ExprCase () (CaseOf $1 $2 $3 (pure ($5, Unconditional $6 $8))) }
  | 'case' sep(expr, ',') 'of' '\{' sep(binder1, ',') '\}' guardedCase
      { ExprCase () (CaseOf $1 $2 $3 (pure ($5, $7))) }

expr6 :: { Expr () }
  : expr7 %shift { $1 }
  | expr7 '{' '}' { ExprApp () $1 (ExprRecord () (Wrapped $2 Nothing $3)) }
  | expr7 '{' sep(recordUpdateOrLabel, ',') '}'
      {% toRecordFields $3 >>= \case
          Left xs -> pure $ ExprApp () $1 (ExprRecord () (Wrapped $2 (Just xs) $4))
          Right xs -> pure $ ExprRecordUpdate () $1 (Wrapped $2 xs $4)
      }

expr7 :: { Expr () }
  : exprAtom { $1 }
  | exprAtom '.' sep(label, '.') { ExprRecordAccessor () (RecordAccessor $1 $2 $3) }

exprAtom :: { Expr () }
  : '_' { ExprSection () $1 }
  | hole { ExprHole () $1 }
  | qualIdent { ExprIdent () $1 }
  | qualProperName { ExprConstructor () (getQualifiedProperName $1) }
  | qualSymbol { ExprOpName () (getQualifiedOpName $1) }
  | boolean { uncurry (ExprBoolean ()) $1 }
  | char { uncurry (ExprChar ()) $1 }
  | string { uncurry (ExprString ()) $1 }
  | number { uncurry (ExprNumber ()) $1 }
  | delim('[', expr, ',', ']') { ExprArray () $1 }
  | delim('{', recordLabel, ',', '}') { ExprRecord () $1 }
  | '(' expr ')' { ExprParens () (Wrapped $1 $2 $3) }

recordLabel :: { RecordLabeled (Expr ()) }
  : label {% fmap RecordPun . toName Ident $ lblTok $1 }
  | label '=' expr {% addFailure [$2] ErrRecordUpdateInCtr *> pure (RecordPun $ unexpectedName $ lblTok $1) }
  | label ':' expr { RecordField $1 $2 $3 }

recordUpdateOrLabel :: { Either (RecordLabeled (Expr ())) (RecordUpdate ()) }
  : label ':' expr { Left (RecordField $1 $2 $3) }
  | label {% fmap (Left . RecordPun) . toName Ident $ lblTok $1 }
  | label '=' expr { Right (RecordUpdateLeaf $1 $2 $3) }
  | label '{' sep(recordUpdate, ',') '}' { Right (RecordUpdateBranch $1 (Wrapped $2 $3 $4)) }

recordUpdate :: { RecordUpdate () }
  : label '=' expr { RecordUpdateLeaf $1 $2 $3 }
  | label '{' sep(recordUpdate, ',') '}' { RecordUpdateBranch $1 (Wrapped $2 $3 $4) }

letBinding :: { LetBinding () }
  : ident '::' type { LetBindingSignature () (Labeled $1 $2 $3) }
  | ident guardedDecl { LetBindingName () (ValueBindingFields $1 [] $2) }
  | ident many(binderAtom) guardedDecl { LetBindingName () (ValueBindingFields $1 (NE.toList $2) $3) }
  | binder1 '=' exprWhere { LetBindingPattern () $1 $2 $3 }
  | typeHead '=' type {% checkNoWildcards $3 *> pure (LetBindingType () $1 $2 $3) }
  | 'type' properName '::' type {% checkNoWildcards $4 *> pure (LetBindingKindSignature () $1 (Labeled (getProperName $2) $3 $4)) }

caseBranch :: { (Separated (Binder ()), Guarded ()) }
  : sep(binder1, ',') guardedCase { ($1, $2) }

guardedDecl :: { Guarded () }
  : '=' exprWhere { Unconditional $1 $2 }
  | many(guardedDeclExpr) { Guarded $1 }

guardedDeclExpr :: { GuardedExpr () }
  : guard '=' exprWhere { uncurry GuardedExpr $1 $2 $3 }

guardedCase :: { Guarded () }
  : '->' exprWhere { Unconditional $1 $2 }
  | many(guardedCaseExpr) { Guarded $1 }

guardedCaseExpr :: { GuardedExpr () }
  : guard '->' exprWhere { uncurry GuardedExpr $1 $2 $3 }

-- Do/Ado statements and pattern guards require unbounded lookahead due to many
-- conflicts between `binder` and `expr` syntax. For example `Foo a b c` can
-- either be a constructor `binder` or several `expr` applications, and we won't
-- know until we see a `<-` or layout separator.
--
-- One way to resolve this would be to parse a `binder` as an `expr` and then
-- reassociate it after the fact. However this means we can't use the `binder`
-- productions to parse it, so we'd have to maintain an ad-hoc handwritten
-- parser which is very difficult to audit.
--
-- As an alternative we introduce some backtracking. Using %partial parsers and
-- monadic reductions, we can invoke productions manually and use the
-- backtracking `tryPrefix` combinator. Binders are generally very short in
-- comparison to expressions, so the cost is modest.
--
--     doBlock
--       : 'do' '\{' manySep(doStatement, '\;') '\}'
--
--     doStatement
--       : 'let' '\{' manySep(letBinding, '\;') '\}'
--       | expr
--       | binder '<-' expr
--
--     guard
--       : '|' sep(patternGuard, ',')
--
--     patternGuard
--       : expr1
--       | binder '<-' expr1
--
doBlock :: { DoBlock () }
  : 'do' '\{'
      {%% revert $ do
        res <- parseDoStatement
        when (null res) $ addFailure [$2] ErrEmptyDo
        pure $ DoBlock $1 $ NE.fromList res
      }

adoBlock :: { (SourceToken, [DoStatement ()]) }
  : 'ado' '\{' '\}' { ($1, []) }
  | 'ado' '\{'
      {%% revert $ fmap ($1,) parseDoStatement }

doStatement :: { [DoStatement ()] }
  : 'let' '\{' manySep(letBinding, '\;') '\}'
      {%^ revert $ fmap (DoLet $1 $3 :) parseDoNext }
  | {- empty -}
      {%^ revert $ do
        stmt <- tryPrefix parseBinderAndArrow parseDoExpr
        let
          ctr = case stmt of
            (Just (binder, sep), expr) ->
              (DoBind binder sep expr :)
            (Nothing, expr) ->
              (DoDiscard expr :)
        fmap ctr parseDoNext
      }

doExpr :: { Expr () }
  : expr {%^ revert $ pure $1 }

doNext :: { [DoStatement ()] }
  : '\;' {%^ revert parseDoStatement }
  | '\}' {%^ revert $ pure [] }

guard :: { (SourceToken, Separated (PatternGuard ())) }
  : '|' {%% revert $ fmap (($1,) . uncurry Separated) parseGuardStatement }

guardStatement :: { (PatternGuard (), [(SourceToken, PatternGuard ())]) }
  : {- empty -}
      {%^ revert $ do
        grd <- fmap (uncurry PatternGuard) $ tryPrefix parseBinderAndArrow parseGuardExpr
        fmap (grd,) parseGuardNext
      }

guardExpr :: { Expr() }
  : expr1 {%^ revert $ pure $1 }

guardNext :: { [(SourceToken, PatternGuard ())] }
  : ',' {%^ revert $ fmap (\(g, gs) -> ($1, g) : gs) parseGuardStatement }
  | {- empty -} {%^ revert $ pure [] }

binderAndArrow :: { (Binder (), SourceToken) }
  : binder '<-' {%^ revert $ pure ($1, $2) }

binder :: { Binder () }
  : binder1 { $1 }
  | binder1 '::' type { BinderTyped () $1 $2 $3 }

binder1 :: { Binder () }
  : binder2 { $1 }
  | binder1 qualOp binder2 { BinderOp () $1 (getQualifiedOpName $2) $3 }

binder2 :: { Binder () }
  : many(binderAtom) {% toBinderConstructor $1 }
  | '-' number { uncurry (BinderNumber () (Just $1)) $2 }

binderAtom :: { Binder () }
  : '_' { BinderWildcard () $1 }
  | ident %shift { BinderVar () $1 }
  | ident '@' binderAtom { BinderNamed () $1 $2 $3 }
  | qualProperName { BinderConstructor () (getQualifiedProperName $1) [] }
  | boolean { uncurry (BinderBoolean ()) $1 }
  | char { uncurry (BinderChar ()) $1 }
  | string { uncurry (BinderString ()) $1 }
  | number { uncurry (BinderNumber () Nothing) $1 }
  | delim('[', binder, ',', ']') { BinderArray () $1 }
  | delim('{', recordBinder, ',', '}') { BinderRecord () $1 }
  | '(' binder ')' { BinderParens () (Wrapped $1 $2 $3) }

recordBinder :: { RecordLabeled (Binder ()) }
  : label {% fmap RecordPun . toName Ident $ lblTok $1 }
  | label '=' binder {% addFailure [$2] ErrRecordUpdateInCtr *> pure (RecordPun $ unexpectedName $ lblTok $1) }
  | label ':' binder { RecordField $1 $2 $3 }

-- By splitting up the module header from the body, we can incrementally parse
-- just the header, and then continue parsing the body while still sharing work.
moduleHeader :: { Module () }
  : 'module' moduleName exports 'where' '\{' moduleImports
      { (Module () $1 $2 $3 $4 $6 [] []) }

moduleBody :: { ([Declaration ()], [Comment LineFeed]) }
  : moduleDecls '\}'
      {%^ \(SourceToken ann _) -> pure (snd $1, tokLeadingComments ann) }

moduleImports :: { [ImportDecl ()] }
  : importDecls importDecl '\}'
      {%^ revert $ pushBack $3 *> pure (reverse ($2 : $1)) }
  | importDecls
      {%^ revert $ pure (reverse $1) }

importDecls :: { [ImportDecl ()] }
  : importDecls importDecl '\;' { $2 : $1 }
  | {- empty -} { [] }

moduleDecls :: { ([ImportDecl ()], [Declaration ()]) }
  : manySep(moduleDecl, '\;') {% toModuleDecls $ NE.toList $1 }
  | {- empty -} { ([], []) }

moduleDecl :: { TmpModuleDecl () }
  : importDecl { TmpImport $1 }
  | sep(decl, declElse) { TmpChain $1 }

declElse :: { SourceToken }
  : 'else' { $1 }
  | 'else' '\;' { $1 }

exports :: { Maybe (DelimitedNonEmpty (Export ())) }
  : {- empty -} { Nothing }
  | '(' sep(export, ',') ')' { Just (Wrapped $1 $2 $3) }

export :: { Export () }
  : ident { ExportValue () $1 }
  | symbol { ExportOp () (getOpName $1) }
  | properName { ExportType () (getProperName $1) Nothing }
  | properName dataMembers { ExportType () (getProperName $1) (Just $2) }
  | 'type' symbol { ExportTypeOp () $1 (getOpName $2) }
  | 'class' properName { ExportClass () $1 (getProperName $2) }
  | 'kind' properName {% addWarning [$1, nameTok (getProperName $2)] WarnDeprecatedKindExportSyntax *> pure (ExportKind () $1 (getProperName $2)) }
  | 'module' moduleName { ExportModule () $1 $2 }

dataMembers :: { (DataMembers ()) }
 : '(..)' { DataAll () $1 }
 | '(' ')' { DataEnumerated () (Wrapped $1 Nothing $2) }
 | '(' sep(properName, ',') ')' { DataEnumerated () (Wrapped $1 (Just \$ getProperName <\$> $2) $3) }

importDecl :: { ImportDecl () }
  : 'import' moduleName imports { ImportDecl () $1 $2 $3 Nothing }
  | 'import' moduleName imports 'as' moduleName { ImportDecl () $1 $2 $3 (Just ($4, $5)) }

imports :: { Maybe (Maybe SourceToken, DelimitedNonEmpty (Import ())) }
  : {- empty -} { Nothing }
  | '(' sep(import, ',') ')' { Just (Nothing, Wrapped $1 $2 $3) }
  | 'hiding' '(' sep(import, ',') ')' { Just (Just $1, Wrapped $2 $3 $4) }

import :: { Import () }
  : ident { ImportValue () $1 }
  | symbol { ImportOp () (getOpName $1) }
  | properName { ImportType () (getProperName $1) Nothing }
  | properName dataMembers { ImportType () (getProperName $1) (Just $2) }
  | 'type' symbol { ImportTypeOp () $1 (getOpName $2) }
  | 'class' properName { ImportClass () $1 (getProperName $2) }
  | 'kind' properName {% addWarning [$1, nameTok (getProperName $2)] WarnDeprecatedKindImportSyntax *> pure (ImportKind () $1 (getProperName $2)) }

decl :: { Declaration () }
  : dataHead { DeclData () $1 Nothing }
  | dataHead '=' sep(dataCtor, '|') { DeclData () $1 (Just ($2, $3)) }
  | typeHead '=' type {% checkNoWildcards $3 *> pure (DeclType () $1 $2 $3) }
  | newtypeHead '=' properName typeAtom {% checkNoWildcards $4 *> pure (DeclNewtype () $1 $2 (getProperName $3) $4) }
  | classHead { either id (\h -> DeclClass () h Nothing) $1 }
  | classHead 'where' '\{' manySep(classMember, '\;') '\}' {% either (const (parseError $2)) (\h -> pure $ DeclClass () h (Just ($2, $4))) $1 }
  | instHead { DeclInstanceChain () (Separated (Instance $1 Nothing) []) }
  | instHead 'where' '\{' manySep(instBinding, '\;') '\}' { DeclInstanceChain () (Separated (Instance $1 (Just ($2, $4))) []) }
  | 'data' properName '::' type {% checkNoWildcards $4 *> pure (DeclKindSignature () $1 (Labeled (getProperName $2) $3 $4)) }
  | 'newtype' properName '::' type {% checkNoWildcards $4 *> pure (DeclKindSignature () $1 (Labeled (getProperName $2) $3 $4)) }
  | 'type' properName '::' type {% checkNoWildcards $4 *> pure (DeclKindSignature () $1 (Labeled (getProperName $2) $3 $4)) }
  | 'derive' instHead { DeclDerive () $1 Nothing $2 }
  | 'derive' 'newtype' instHead { DeclDerive () $1 (Just $2) $3 }
  | ident '::' type { DeclSignature () (Labeled $1 $2 $3) }
  | ident manyOrEmpty(binderAtom) guardedDecl { DeclValue () (ValueBindingFields $1 $2 $3) }
  | fixity { DeclFixity () $1 }
  | 'foreign' 'import' ident '::' type {% when (isConstrained $5) (addWarning ([$1, $2, nameTok $3, $4] <> toList (flattenType $5)) WarnDeprecatedConstraintInForeignImportSyntax) *> pure (DeclForeign () $1 $2 (ForeignValue (Labeled $3 $4 $5))) }
  | 'foreign' 'import' 'data' properName '::' type { DeclForeign () $1 $2 (ForeignData $3 (Labeled (getProperName $4) $5 $6)) }
  | 'foreign' 'import' 'kind' properName {% addWarning [$1, $2, $3, nameTok (getProperName $4)] WarnDeprecatedForeignKindSyntax *> pure (DeclForeign () $1 $2 (ForeignKind $3 (getProperName $4))) }
  | 'type' 'role' properName many(role) { DeclRole () $1 $2 (getProperName $3) $4 }

dataHead :: { DataHead () }
  : 'data' properName manyOrEmpty(typeVarBinding) { DataHead $1 (getProperName $2) $3 }

typeHead :: { DataHead () }
  : 'type' properName manyOrEmpty(typeVarBinding) { DataHead $1 (getProperName $2) $3 }

newtypeHead :: { DataHead () }
  : 'newtype' properName manyOrEmpty(typeVarBinding) { DataHead $1 (getProperName $2) $3 }

dataCtor :: { DataCtor () }
  : properName manyOrEmpty(typeAtom)
      {% for_ $2 checkNoWildcards *> pure (DataCtor () (getProperName $1) $2) }

-- Class head syntax requires unbounded lookahead due to a conflict between
-- row syntax and `typeVarBinding`. `(a :: B)` is either a row in `constraint`
-- where `B` is a type or a `typeVarBinding` where `B` is a kind. We must see
-- either a `<=`, `where`, or layout delimiter before deciding which it is.
--
--     classHead
--       : 'class' classNameAndFundeps
--       | 'class' constraints '<=' classNameAndFundeps
--
classHead :: { Either (Declaration ()) (ClassHead ()) }
  : 'class'
      {%% revert $ oneOf $ NE.fromList
          [ fmap (Left . DeclKindSignature () $1) parseClassSignature
          , do
              (super, (name, vars, fundeps)) <- tryPrefix parseClassSuper parseClassNameAndFundeps
              let hd = ClassHead $1 super name vars fundeps
              checkFundeps hd
              pure $ Right hd
          ]
      }

classSignature :: { Labeled (Name (N.ProperName 'N.TypeName)) (Type ()) }
  : properName '::' type {%^ revert $ checkNoWildcards $3 *> pure (Labeled (getProperName $1) $2 $3) }

classSuper :: { (OneOrDelimited (Constraint ()), SourceToken) }
  : constraints '<=' {%^ revert $ pure ($1, $2) }

classNameAndFundeps :: { (Name (N.ProperName 'N.ClassName), [TypeVarBinding ()], Maybe (SourceToken, Separated ClassFundep)) }
  : properName manyOrEmpty(typeVarBinding) fundeps {%^ revert $ pure (getProperName $1, $2, $3) }

fundeps :: { Maybe (SourceToken, Separated ClassFundep) }
  : {- empty -} { Nothing }
  | '|' sep(fundep, ',') { Just ($1, $2) }

fundep :: { ClassFundep }
  : '->' many(ident) { FundepDetermined $1 $2 }
  | many(ident) '->' many(ident) { FundepDetermines $1 $2 $3 }

classMember :: { Labeled (Name Ident) (Type ()) }
  : ident '::' type {% checkNoWildcards $3 *> pure (Labeled $1 $2 $3) }

instHead :: { InstanceHead () }
  : 'instance' constraints '=>' qualProperName manyOrEmpty(typeAtom)
      { InstanceHead $1 Nothing (Just ($2, $3)) (getQualifiedProperName $4) $5 }
  | 'instance' qualProperName manyOrEmpty(typeAtom)
      { InstanceHead $1 Nothing Nothing (getQualifiedProperName $2) $3 }
  | 'instance' ident '::' constraints '=>' qualProperName manyOrEmpty(typeAtom)
      { InstanceHead $1 (Just ($2, $3)) (Just ($4, $5)) (getQualifiedProperName $6) $7 }
  | 'instance' ident '::' qualProperName manyOrEmpty(typeAtom)
      { InstanceHead $1 (Just ($2, $3)) Nothing (getQualifiedProperName $4) $5 }

constraints :: { OneOrDelimited (Constraint ()) }
  : constraint { One $1 }
  | '(' sep(constraint, ',') ')' { Many (Wrapped $1 $2 $3) }

constraint :: { Constraint () }
  : qualProperName manyOrEmpty(typeAtom) {% for_ $2 checkNoWildcards *> for_ $2 checkNoForalls *> pure (Constraint () (getQualifiedProperName $1) $2) }
  | '(' constraint ')' { ConstraintParens () (Wrapped $1 $2 $3) }

instBinding :: { InstanceBinding () }
  : ident '::' type { InstanceBindingSignature () (Labeled $1 $2 $3) }
  | ident manyOrEmpty(binderAtom) guardedDecl { InstanceBindingName () (ValueBindingFields $1 $2 $3) }

fixity :: { FixityFields }
  : infix int qualIdent 'as' op { FixityFields $1 $2 (FixityValue (fmap Left $3) $4 (getOpName $5)) }
  | infix int qualProperName 'as' op { FixityFields $1 $2 (FixityValue (fmap Right (getQualifiedProperName $3)) $4 (getOpName $5)) }
  | infix int 'type' qualProperName 'as' op { FixityFields $1 $2 (FixityType $3 (getQualifiedProperName $4) $5 (getOpName $6)) }

infix :: { (SourceToken, Fixity) }
  : 'infix' { ($1, Infix) }
  | 'infixl' { ($1, Infixl) }
  | 'infixr' { ($1, Infixr) }

role :: { Role }
  : 'nominal' { Role $1 R.Nominal }
  | 'representational' { Role $1 R.Representational }
  | 'phantom' { Role $1 R.Phantom }

-- Partial parsers which can be combined with combinators for adhoc use. We need
-- to revert the lookahead token so that it doesn't consume an extra token
-- before succeeding.

importDeclP :: { ImportDecl () }
  : importDecl {%^ revert $ pure $1 }

declP :: { Declaration () }
  : decl {%^ revert $ pure $1 }

exprP :: { Expr () }
  : expr {%^ revert $ pure $1 }

typeP :: { Type () }
  : type {%^ revert $ pure $1 }

moduleNameP :: { Name N.ModuleName }
  : moduleName {%^ revert $ pure $1 }

qualIdentP :: { QualifiedName Ident }
  : qualIdent {%^ revert $ pure $1 }

{
lexer :: (SourceToken -> Parser a) -> Parser a
lexer k = munch >>= k

parse :: Text -> ([ParserWarning], Either (NE.NonEmpty ParserError) (Module ()))
parse = either (([],) . Left) resFull . parseModule . lex

data PartialResult a = PartialResult
  { resPartial :: a
  , resFull :: ([ParserWarning], Either (NE.NonEmpty ParserError) a)
  } deriving (Functor)

parseModule :: [LexResult] -> Either (NE.NonEmpty ParserError) (PartialResult (Module ()))
parseModule toks = fmap (\header -> PartialResult header (parseFull header)) headerRes
  where
  (st, headerRes) =
    runParser (ParserState toks [] []) parseModuleHeader

  parseFull header = do
    let (ParserState _ _ warnings, res) = runParser st parseModuleBody
    (warnings, (\(decls, trailing) -> header { modDecls = decls, modTrailingComments = trailing }) <$> res)
}
