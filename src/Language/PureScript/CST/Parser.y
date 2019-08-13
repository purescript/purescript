{
module Language.PureScript.CST.Parser
  ( parseType
  , parseKind
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
import Data.Foldable (foldl', for_)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Traversable (for)
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types
import Language.PureScript.CST.Utils
import qualified Language.PureScript.Names as N
import Language.PureScript.PSString (PSString)
}

%expect 98

%name parseKind kind
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
%partial parseClassSuper classSuper
%partial parseClassNameAndFundeps classNameAndFundeps
%partial parseBinderAndArrow binderAndArrow
%tokentype { SourceToken }
%monad { Parser }
%error { parseError }
%lexer { lexer } { SourceToken _ TokEof }

%token
  '('             { SourceToken _ TokLeftParen }
  ')'             { SourceToken _ TokRightParen }
  '{'             { SourceToken _ TokLeftBrace }
  '}'             { SourceToken _ TokRightBrace }
  '['             { SourceToken _ TokLeftSquare }
  ']'             { SourceToken _ TokRightSquare }
  '\{'            { SourceToken _ TokLayoutStart }
  '\}'            { SourceToken _ TokLayoutEnd }
  '\;'            { SourceToken _ TokLayoutSep }
  '<-'            { SourceToken _ (TokLeftArrow _) }
  '->'            { SourceToken _ (TokRightArrow _) }
  '<='            { SourceToken _ (TokOperator [] sym) | isLeftFatArrow sym }
  '=>'            { SourceToken _ (TokRightFatArrow _) }
  ':'             { SourceToken _ (TokOperator [] ":") }
  '::'            { SourceToken _ (TokDoubleColon _) }
  '='             { SourceToken _ TokEquals }
  '|'             { SourceToken _ TokPipe }
  '`'             { SourceToken _ TokTick }
  '.'             { SourceToken _ TokDot }
  ','             { SourceToken _ TokComma }
  '_'             { SourceToken _ TokUnderscore }
  '\\'            { SourceToken _ TokBackslash }
  '-'             { SourceToken _ (TokOperator [] "-") }
  '@'             { SourceToken _ (TokOperator [] "@") }
  '#'             { SourceToken _ (TokOperator [] "#") }
  'ado'           { SourceToken _ (TokLowerName _ "ado") }
  'as'            { SourceToken _ (TokLowerName [] "as") }
  'case'          { SourceToken _ (TokLowerName [] "case") }
  'class'         { SourceToken _ (TokLowerName [] "class") }
  'data'          { SourceToken _ (TokLowerName [] "data") }
  'derive'        { SourceToken _ (TokLowerName [] "derive") }
  'do'            { SourceToken _ (TokLowerName _ "do") }
  'else'          { SourceToken _ (TokLowerName [] "else") }
  'false'         { SourceToken _ (TokLowerName [] "false") }
  'forall'        { SourceToken _ (TokForall ASCII) }
  'forallu'       { SourceToken _ (TokForall Unicode) }
  'foreign'       { SourceToken _ (TokLowerName [] "foreign") }
  'hiding'        { SourceToken _ (TokLowerName [] "hiding") }
  'import'        { SourceToken _ (TokLowerName [] "import") }
  'if'            { SourceToken _ (TokLowerName [] "if") }
  'in'            { SourceToken _ (TokLowerName [] "in") }
  'infix'         { SourceToken _ (TokLowerName [] "infix") }
  'infixl'        { SourceToken _ (TokLowerName [] "infixl") }
  'infixr'        { SourceToken _ (TokLowerName [] "infixr") }
  'instance'      { SourceToken _ (TokLowerName [] "instance") }
  'kind'          { SourceToken _ (TokLowerName [] "kind") }
  'let'           { SourceToken _ (TokLowerName [] "let") }
  'module'        { SourceToken _ (TokLowerName [] "module") }
  'newtype'       { SourceToken _ (TokLowerName [] "newtype") }
  'of'            { SourceToken _ (TokLowerName [] "of") }
  'then'          { SourceToken _ (TokLowerName [] "then") }
  'true'          { SourceToken _ (TokLowerName [] "true") }
  'type'          { SourceToken _ (TokLowerName [] "type") }
  'where'         { SourceToken _ (TokLowerName [] "where") }
  '(->)'          { SourceToken _ (TokSymbolArr _) }
  '(..)'          { SourceToken _ (TokSymbolName [] "..") }
  LOWER           { SourceToken _ (TokLowerName [] _) }
  QUAL_LOWER      { SourceToken _ (TokLowerName _ _) }
  UPPER           { SourceToken _ (TokUpperName [] _) }
  QUAL_UPPER      { SourceToken _ (TokUpperName _ _) }
  SYMBOL          { SourceToken _ (TokSymbolName [] _) }
  QUAL_SYMBOL     { SourceToken _ (TokSymbolName _ _) }
  OPERATOR        { SourceToken _ (TokOperator [] _) }
  QUAL_OPERATOR   { SourceToken _ (TokOperator _ _) }
  LIT_HOLE        { SourceToken _ (TokHole _) }
  LIT_CHAR        { SourceToken _ (TokChar _ _) }
  LIT_STRING      { SourceToken _ (TokString _ _) }
  LIT_RAW_STRING  { SourceToken _ (TokRawString _) }
  LIT_INT         { SourceToken _ (TokInt _ _) }
  LIT_NUMBER      { SourceToken _ (TokNumber _ _) }

%%

many(a) :: { NE.NonEmpty a }
  : many1(a) { NE.reverse $1 }

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
  : a { [(placeholder, $1)] }
  | sep1(a, s) s a { ($2, $3) : $1 }

delim(a, b, c, d) :: { Delimited b }
  : a d { Wrapped $1 Nothing $2 }
  | a sep(b, c) d { Wrapped $1 (Just $2) $3 }

moduleName :: { Name N.ModuleName }
  : UPPER {% upperToModuleName $1 }
  | QUAL_UPPER {% upperToModuleName $1 }

qualProperName :: { QualifiedName (N.ProperName a) }
  : UPPER {% toQualifiedName N.ProperName $1 }
  | QUAL_UPPER {% toQualifiedName N.ProperName $1 }

properName :: { Name (N.ProperName a) }
  : UPPER {% toName N.ProperName $1 }

qualIdent :: { QualifiedName Ident }
  : LOWER {% toQualifiedName Ident $1 }
  | QUAL_LOWER {% toQualifiedName Ident $1 }
  | 'as' {% toQualifiedName Ident $1 }
  | 'hiding' {% toQualifiedName Ident $1 }
  | 'kind' {% toQualifiedName Ident $1 }

ident :: { Name Ident }
  : LOWER {% toName Ident $1 }
  | 'as' {% toName Ident $1 }
  | 'hiding' {% toName Ident $1 }
  | 'kind' {% toName Ident $1 }

qualOp :: { QualifiedName (N.OpName a) }
  : OPERATOR {% toQualifiedName N.OpName $1 }
  | QUAL_OPERATOR {% toQualifiedName N.OpName $1 }
  | '<=' {% toQualifiedName N.OpName $1 }
  | '-' {% toQualifiedName N.OpName $1 }
  | '#' {% toQualifiedName N.OpName $1 }
  | ':' {% toQualifiedName N.OpName $1 }

op :: { Name (N.OpName a) }
  : OPERATOR {% toName N.OpName $1 }
  | '<=' {% toName N.OpName $1 }
  | '-' {% toName N.OpName $1 }
  | '#' {% toName N.OpName $1 }
  | ':' {% toName N.OpName $1 }

qualSymbol :: { QualifiedName (N.OpName a) }
  : SYMBOL {% toQualifiedName N.OpName $1 }
  | QUAL_SYMBOL {% toQualifiedName N.OpName $1 }
  | '(..)' {% toQualifiedName N.OpName $1 }

symbol :: { Name (N.OpName a) }
  : SYMBOL {% toName N.OpName $1 }
  | '(..)' {% toName N.OpName $1 }

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
  | 'of' { toLabel $1 }
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

kind :: { Kind () }
  : kind1 { $1 }
  | kind1 '->' kind { KindArr () $1 $2 $3 }

kind1 :: { Kind () }
  : qualProperName { KindName () $1 }
  | '#' kind1 { KindRow () $1 $2 }
  | '(' kind ')' { KindParens () (Wrapped $1 $2 $3) }

type :: { Type () }
  : type1 { $1 }
  | type1 '::' kind { TypeKinded () $1 $2 $3 }

type1 :: { Type () }
  : type2 { $1 }
  | forall many(typeVarBinding) '.' type1 { TypeForall () $1 $2 $3 $4 }

type2 :: { Type () }
  : type3 { $1 }
  | type3 '->' type1 { TypeArr () $1 $2 $3 }
  | type3 '=>' type1 {% do cs <- toConstraint $1; pure $ TypeConstrained () cs $2 $3 }

type3 :: { Type () }
  : type4 { $1 }
  | type3 qualOp type4 { TypeOp () $1 $2 $3 }

type4 :: { Type () }
  : typeAtom { $1 }
  | type4 typeAtom { TypeApp () $1 $2 }

typeAtom :: { Type ()}
  : '_' { TypeWildcard () $1 }
  | ident { TypeVar () $1 }
  | qualProperName { TypeConstructor () $1 }
  | qualSymbol { TypeOpName () $1 }
  | string { uncurry (TypeString ()) $1 }
  | hole { TypeHole () $1 }
  | '(->)' { TypeArrName () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' type1 ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' kind ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

-- Due to a conflict between row syntax and kinded type syntax, we require
-- kinded type variables to be wrapped in parens. Thus `(a :: Foo)` is always a
-- row, and to annotate `a` with kind `Foo`, one must use `((a) :: Foo)`.
typeKindedAtom :: { Type () }
  : '_' { TypeWildcard () $1 }
  | qualProperName { TypeConstructor () $1 }
  | qualSymbol { TypeOpName () $1 }
  | hole { TypeHole () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' type1 ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' kind ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

row :: { Row () }
  : {- empty -} { Row Nothing Nothing }
  | '|' type { Row Nothing (Just ($1, $2)) }
  | sep(rowLabel, ',') { Row (Just $1) Nothing }
  | sep(rowLabel, ',') '|' type { Row (Just $1) (Just ($2, $3)) }

rowLabel :: { Labeled Label (Type ()) }
  : label '::' type { Labeled $1 $2 $3 }

typeVarBinding :: { TypeVarBinding () }
  : ident { TypeVarName $1 }
  | '(' ident '::' kind ')' { TypeVarKinded (Wrapped $1 (Labeled $2 $3 $4) $5) }

forall :: { SourceToken }
  : 'forall' { $1 }
  | 'forallu' { $1 }

exprWhere :: { Where () }
  : expr { Where $1 Nothing }
  | expr 'where' '\{' manySep(letBinding, '\;') '\}' { Where $1 (Just ($2, $4)) }

expr :: { Expr () }
  : expr1 { $1 }
  | expr1 '::' type { ExprTyped () $1 $2 $3 }

expr1 :: { Expr () }
  : expr2 { $1 }
  | expr1 qualOp expr2 { ExprOp () $1 $2 $3 }

expr2 :: { Expr () }
  : expr3 { $1 }
  | expr2 '`' exprBacktick '`' expr3 { ExprInfix () $1 (Wrapped $2 $3 $4) $5 }

exprBacktick :: { Expr () }
  : expr3 { $1 }
  | exprBacktick qualOp expr3 { ExprOp () $1 $2 $3 }

expr3 :: { Expr () }
  : expr4 { $1 }
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
  : expr7 { $1 }
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
  | qualProperName { ExprConstructor () $1 }
  | qualSymbol { ExprOpName () $1 }
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
  | binder1 qualOp binder2 { BinderOp () $1 $2 $3 }

binder2 :: { Binder () }
  : many(binderAtom) {% toBinderConstructor $1 }

binderAtom :: { Binder () }
  : '_' { BinderWildcard () $1 }
  | ident { BinderVar () $1 }
  | ident '@' binderAtom { BinderNamed () $1 $2 $3 }
  | qualProperName { BinderConstructor () $1 [] }
  | boolean { uncurry (BinderBoolean ()) $1 }
  | char { uncurry (BinderChar ()) $1 }
  | string { uncurry (BinderString ()) $1 }
  | number { uncurry (BinderNumber () Nothing) $1 }
  | '-' number { uncurry (BinderNumber () (Just $1)) $2 }
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

moduleDecl :: { TmpModuleDecl a }
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
  | symbol { ExportOp () $1 }
  | properName { ExportType () $1 Nothing }
  | properName dataMembers { ExportType () $1 (Just $2) }
  | 'type' symbol { ExportTypeOp () $1 $2 }
  | 'class' properName { ExportClass () $1 $2 }
  | 'kind' properName { ExportKind () $1 $2 }
  | 'module' moduleName { ExportModule () $1 $2 }

dataMembers :: { (DataMembers ()) }
 : '(..)' { DataAll () $1 }
 | '(' ')' { DataEnumerated () (Wrapped $1 Nothing $2) }
 | '(' sep(properName, ',') ')' { DataEnumerated () (Wrapped $1 (Just $2) $3) }

importDecl :: { ImportDecl () }
  : 'import' moduleName imports { ImportDecl () $1 $2 $3 Nothing }
  | 'import' moduleName imports 'as' moduleName { ImportDecl () $1 $2 $3 (Just ($4, $5)) }

imports :: { Maybe (Maybe SourceToken, DelimitedNonEmpty (Import ())) }
  : {- empty -} { Nothing }
  | '(' sep(import, ',') ')' { Just (Nothing, Wrapped $1 $2 $3) }
  | 'hiding' '(' sep(import, ',') ')' { Just (Just $1, Wrapped $2 $3 $4) }

import :: { Import () }
  : ident { ImportValue () $1 }
  | symbol { ImportOp () $1 }
  | properName { ImportType () $1 Nothing }
  | properName dataMembers { ImportType () $1 (Just $2) }
  | 'type' symbol { ImportTypeOp () $1 $2 }
  | 'class' properName { ImportClass () $1 $2 }
  | 'kind' properName { ImportKind () $1 $2 }

decl :: { Declaration () }
  : dataHead { DeclData () $1 Nothing }
  | dataHead '=' sep(dataCtor, '|') { DeclData () $1 (Just ($2, $3)) }
  | typeHead '=' type {% checkNoWildcards $3 *> pure (DeclType () $1 $2 $3) }
  | newtypeHead '=' properName typeAtom {% checkNoWildcards $4 *> pure (DeclNewtype () $1 $2 $3 $4) }
  | classHead {% checkFundeps $1 *> pure (DeclClass () $1 Nothing) }
  | classHead 'where' '\{' manySep(classMember, '\;') '\}' {% checkFundeps $1 *> pure (DeclClass () $1 (Just ($2, $4))) }
  | instHead { DeclInstanceChain () (Separated (Instance $1 Nothing) []) }
  | instHead 'where' '\{' manySep(instBinding, '\;') '\}' { DeclInstanceChain () (Separated (Instance $1 (Just ($2, $4))) []) }
  | 'derive' instHead { DeclDerive () $1 Nothing $2 }
  | 'derive' 'newtype' instHead { DeclDerive () $1 (Just $2) $3 }
  | ident '::' type { DeclSignature () (Labeled $1 $2 $3) }
  | ident manyOrEmpty(binderAtom) guardedDecl { DeclValue () (ValueBindingFields $1 $2 $3) }
  | fixity { DeclFixity () $1 }
  | 'foreign' 'import' foreign { DeclForeign () $1 $2 $3 }

dataHead :: { DataHead () }
  : 'data' properName manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

typeHead :: { DataHead () }
  : 'type' properName manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

newtypeHead :: { DataHead () }
  : 'newtype' properName manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

dataCtor :: { DataCtor () }
  : properName manyOrEmpty(typeAtom)
      {% for_ $2 checkNoWildcards *> pure (DataCtor () $1 $2) }

-- Class head syntax requires unbounded lookahead due to a conflict between
-- row syntax and `typeVarBinding`. `(a :: B)` is either a row in `constraint`
-- where `B` is a type or a `typeVarBinding` where `B` is a kind. We must see
-- either a `<=`, `where`, or layout delimiter before deciding which it is.
--
--     classHead
--       : 'class' classNameAndFundeps
--       | 'class' constraints '<=' classNameAndFundeps
--
classHead :: { ClassHead () }
  : 'class'
      {%% revert $ do
        let
          ctr (super, (name, vars, fundeps)) =
            ClassHead $1 super name vars fundeps
        fmap ctr $ tryPrefix parseClassSuper parseClassNameAndFundeps
      }

classSuper
  : constraints '<=' {%^ revert $ pure ($1, $2) }

classNameAndFundeps :: { (Name (N.ProperName 'N.ClassName), [TypeVarBinding ()], Maybe (SourceToken, Separated ClassFundep)) }
  : properName manyOrEmpty(typeVarBinding) fundeps {%^ revert $ pure ($1, $2, $3) }

fundeps :: { Maybe (SourceToken, Separated ClassFundep) }
  : {- empty -} { Nothing }
  | '|' sep(fundep, ',') { Just ($1, $2) }

fundep :: { ClassFundep }
  : '->' many(ident) { FundepDetermined $1 $2 }
  | many(ident) '->' many(ident) { FundepDetermines $1 $2 $3 }

classMember :: { Labeled (Name Ident) (Type ()) }
  : ident '::' type {% checkNoWildcards $3 *> pure (Labeled $1 $2 $3) }

instHead :: { InstanceHead () }
  : 'instance' ident '::' constraints '=>' qualProperName manyOrEmpty(typeAtom)
      { InstanceHead $1 $2 $3 (Just ($4, $5)) $6 $7 }
  | 'instance' ident '::' qualProperName manyOrEmpty(typeAtom)
      { InstanceHead $1 $2 $3 Nothing $4 $5 }

constraints :: { OneOrDelimited (Constraint ()) }
  : constraint { One $1 }
  | '(' sep(constraint, ',') ')' { Many (Wrapped $1 $2 $3) }

constraint :: { Constraint () }
  : qualProperName manyOrEmpty(typeAtom) {% for_ $2 checkNoWildcards *> for_ $2 checkNoForalls *> pure (Constraint () $1 $2) }
  | '(' constraint ')' { ConstraintParens () (Wrapped $1 $2 $3) }

instBinding :: { InstanceBinding () }
  : ident '::' type { InstanceBindingSignature () (Labeled $1 $2 $3) }
  | ident manyOrEmpty(binderAtom) guardedDecl { InstanceBindingName () (ValueBindingFields $1 $2 $3) }

fixity :: { FixityFields }
  : infix int qualIdent 'as' op { FixityFields $1 $2 (FixityValue (fmap Left $3) $4 $5) }
  | infix int qualProperName 'as' op { FixityFields $1 $2 (FixityValue (fmap Right $3) $4 $5) }
  | infix int 'type' qualProperName 'as' op { FixityFields $1 $2 (FixityType $3 $4 $5 $6) }

infix :: { (SourceToken, Fixity) }
  : 'infix' { ($1, Infix) }
  | 'infixl' { ($1, Infixl) }
  | 'infixr' { ($1, Infixr) }

foreign :: { Foreign () }
  : ident '::' type { ForeignValue (Labeled $1 $2 $3) }
  | 'data' properName '::' kind { ForeignData $1 (Labeled $2 $3 $4) }
  | 'kind' properName { ForeignKind $1 $2 }

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

parse :: Text -> Either (NE.NonEmpty ParserError) (Module ())
parse = resFull <=< parseModule . lex

data PartialResult a = PartialResult
  { resPartial :: a
  , resFull :: Either (NE.NonEmpty ParserError) a
  } deriving (Functor)

parseModule :: [LexResult] -> Either (NE.NonEmpty ParserError) (PartialResult (Module ()))
parseModule toks = fmap (\header -> PartialResult header (parseFull header)) headerRes
  where
  (st, headerRes) =
    runParser (ParserState (toks) []) parseModuleHeader

  parseFull header = do
    (decls, trailing) <- snd $ runParser st parseModuleBody
    pure $ header
      { modDecls = decls
      , modTrailingComments = trailing
      }
}
