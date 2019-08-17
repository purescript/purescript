-- | The parser itself is unaware of indentation, and instead only parses explicit
-- delimiters which are inserted by this layout algorithm (much like Haskell).
-- This is convenient because the actual grammar can be specified apart from the
-- indentation rules. Haskell has a few problematic productions which make it
-- impossible to implement a purely lexical layout algorithm, so it also has an
-- additional (and somewhat contentious) parser error side condition. PureScript
-- does not have these problematic productions (particularly foo, bar ::
-- SomeType syntax in declarations), but it does have a few gotchas of it's own.
-- The algorithm is "non-trivial" to say the least, but it is implemented as a
-- purely lexical delimiter parser on a token-by-token basis, which is highly
-- convenient, since it can be replicated in any language or toolchain. There is
-- likely room to simplify it, but there are some seemingly innocuous things
-- that complicate it.
--
-- "Naked" commas (case, patterns, guards, fundeps) are a constant source of
-- complexity, and indeed too much of this is what prevents Haskell from having
-- such an algorithm. Unquoted properties for layout keywords introduce a domino
-- effect of complexity since we have to mask and unmask any usage of . (also in
-- foralls!) or labels in record literals.

module Language.PureScript.CST.Layout where

import Prelude

import Data.DList (snoc)
import qualified Data.DList as DList
import Data.Foldable (find)
import Data.Function ((&))
import Language.PureScript.CST.Types

type LayoutStack = [(SourcePos, LayoutDelim)]

data LayoutDelim
  = LytRoot
  | LytTopDecl
  | LytTopDeclHead
  | LytDeclGuard
  | LytCase
  | LytCaseBinders
  | LytCaseGuard
  | LytLambdaBinders
  | LytParen
  | LytBrace
  | LytSquare
  | LytIf
  | LytThen
  | LytProperty
  | LytForall
  | LytTick
  | LytLet
  | LytLetStmt
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo
  deriving (Show, Eq, Ord)

isIndented :: LayoutDelim -> Bool
isIndented = \case
  LytLet     -> True
  LytLetStmt -> True
  LytWhere   -> True
  LytOf      -> True
  LytDo      -> True
  LytAdo     -> True
  _          -> False

isTopDecl :: SourcePos -> LayoutStack -> Bool
isTopDecl tokPos = \case
  [(lytPos, LytWhere), (_, LytRoot)]
    | srcColumn tokPos == srcColumn lytPos -> True
  _ -> False

lytToken :: SourcePos -> Token -> SourceToken
lytToken pos = SourceToken ann
  where
  ann = TokenAnn
    { tokRange = SourceRange pos pos
    , tokLeadingComments = []
    , tokTrailingComments = []
    }

insertLayout :: SourceToken -> SourcePos -> LayoutStack -> (LayoutStack, [SourceToken])
insertLayout src@(SourceToken tokAnn tok) nextPos stack =
  DList.toList <$> insert (stack, mempty)
  where
  tokPos =
    srcStart $ tokRange tokAnn

  insert state@(stk, acc) = case tok of
    -- `data` declarations need masking (LytTopDecl) because the usage of `|`
    -- should not introduce a LytDeclGard context.
    TokLowerName [] "data" ->
      case state & insertDefault of
        state'@(stk', _) | isTopDecl tokPos stk' ->
          state' & pushStack tokPos LytTopDecl
        state' ->
          state' & popStack (== LytProperty)

    -- `class` declaration heads need masking (LytTopDeclHead) because the
    -- usage of commas in functional dependencies.
    TokLowerName [] "class" ->
      case state & insertDefault of
        state'@(stk', _) | isTopDecl tokPos stk' ->
          state' & pushStack tokPos LytTopDeclHead
        state' ->
          state' & popStack (== LytProperty)

    TokLowerName [] "where" ->
      case stk of
        (_, LytTopDeclHead) : stk' ->
          (stk', acc) & insertToken src & insertStart LytWhere
        (_, LytProperty) : stk' ->
          (stk', acc) & insertToken src
        _ ->
          state & collapse whereP & insertToken src & insertStart LytWhere
      where
      -- `where` always closes do blocks:
      --     example = do do do do foo where foo = ...
      --
      -- `where` closes layout contexts even when indented at the same level:
      --     example = case
      --       Foo -> ...
      --       Bar -> ...
      --       where foo = ...
      whereP _      LytDo = True
      whereP lytPos lyt   = offsideEndP lytPos lyt

    TokLowerName [] "in" ->
      case collapse inP state of
        -- `let/in` is not allowed in `ado` syntax. `in` is treated as a
        -- delimiter and must always close the `ado`.
        --    example = ado
        --      foo <- ...
        --      let bar = ...
        --      in ...
        ((_, LytLetStmt) : (_, LytAdo) : stk', acc') ->
          (stk', acc') & insertEnd & insertEnd & insertToken src
        ((_, lyt) : stk', acc') | isIndented lyt ->
          (stk', acc') & insertEnd & insertToken src
        _ ->
          state & insertDefault & popStack (== LytProperty)
      where
      inP _ LytLet = False
      inP _ LytAdo = False
      inP _ lyt    = isIndented lyt

    TokLowerName [] "let" ->
      state & insertKwProperty next
      where
      next state'@(stk', _) = case stk' of
        (p, LytDo) : _ | srcColumn p == srcColumn tokPos ->
          state' & insertStart LytLetStmt
        (p, LytAdo) : _ | srcColumn p == srcColumn tokPos ->
          state' & insertStart LytLetStmt
        _ ->
          state' & insertStart LytLet

    TokLowerName _ "do" ->
      state & insertKwProperty (insertStart LytDo)

    TokLowerName _ "ado" ->
      state & insertKwProperty (insertStart LytAdo)

    -- `case` heads need masking due to commas.
    TokLowerName [] "case" ->
      state & insertKwProperty (pushStack tokPos LytCase)

    TokLowerName [] "of" ->
      case collapse indentedP state of
        -- When `of` is matched with a `case`, we are in a case block, and we
        -- need to mask additional contexts (LytCaseBinders, LytCaseGuards)
        -- due to commas.
        ((_, LytCase) : stk', acc') ->
          (stk', acc') & insertToken src & insertStart LytOf & pushStack nextPos LytCaseBinders
        state' ->
          state' & insertDefault & popStack (== LytProperty)

    -- `if/then/else` is considered a delimiter context. This allows us to
    -- write chained expressions in `do` blocks without stair-stepping:
    --     example = do
    --       foo
    --       if ... then
    --         ...
    --       else if ... then
    --         ...
    --       else
    --         ...
    TokLowerName [] "if" ->
      state & insertKwProperty (pushStack tokPos LytIf)

    TokLowerName [] "then" ->
      case state & collapse indentedP of
        ((_, LytIf) : stk', acc') ->
          (stk', acc') & insertToken src & pushStack tokPos LytThen
        _ ->
          state & insertDefault & popStack (== LytProperty)

    TokLowerName [] "else" ->
      case state & collapse indentedP of
        ((_, LytThen) : stk', acc') ->
          (stk', acc') & insertToken src
        _ ->
          -- We don't want to insert a layout separator for top-level `else` in
          -- instance chains.
          case state & collapse offsideP of
            state'@(stk', _) | isTopDecl tokPos stk' ->
              state' & insertToken src
            state' ->
              state' & insertSep & insertToken src & popStack (== LytProperty)

    -- `forall` binders need masking because the usage of `.` should not
    -- introduce a LytProperty context.
    TokForall _ ->
      state & insertKwProperty (pushStack tokPos LytForall)

    -- Lambdas need masking because the usage of `->` should not close a
    -- LytDeclGaurd or LytCaseGuard context.
    TokBackslash ->
      state & insertDefault & pushStack tokPos LytLambdaBinders

    TokRightArrow _ ->
      state & collapse arrowP & popStack guardP & insertToken src
      where
      arrowP _      LytDo     = True
      arrowP _      LytOf     = False
      arrowP lytPos lyt       = offsideEndP lytPos lyt

      guardP LytCaseBinders   = True
      guardP LytCaseGuard     = True
      guardP LytLambdaBinders = True
      guardP _                = False

    TokEquals ->
      case state & collapse equalsP of
        ((_, LytDeclGuard) : stk', acc') ->
          (stk', acc') & insertToken src
        _ ->
          state & insertDefault
      where
      equalsP _ LytWhere   = True
      equalsP _ LytLet     = True
      equalsP _ LytLetStmt = True
      equalsP _ _          = False

    -- Guards need masking because of commas.
    TokPipe ->
      case collapse offsideEndP state of
        state'@((_, LytOf) : _, _) ->
          state' & pushStack tokPos LytCaseGuard & insertToken src
        state'@((_, LytLet) : _, _) ->
          state' & pushStack tokPos LytDeclGuard & insertToken src
        state'@((_, LytLetStmt) : _, _) ->
          state' & pushStack tokPos LytDeclGuard & insertToken src
        state'@((_, LytWhere) : _, _) ->
          state' & pushStack tokPos LytDeclGuard & insertToken src
        _ ->
          state & insertDefault

    -- Ticks can either start or end an infix expression. We preemptively
    -- collapse all indentation contexts in search of a starting delimiter,
    -- and backtrack if we don't find one.
    TokTick ->
      case state & collapse indentedP of
        ((_, LytTick) : stk', acc') ->
          (stk', acc') & insertToken src
        _ ->
          state & insertDefault & pushStack tokPos LytTick

    -- In gneral, commas should close all indented contexts.
    --     example = [ do foo
    --                    bar, baz ]
    TokComma ->
      case state & collapse indentedP of
        -- If we see a LytBrace, then we are in a record type or literal.
        -- Record labels need masking so we can use unquoted keywords as labels
        -- without accidentally littering layout delimiters.
        state'@((_, LytBrace) : _, _) ->
          state' & insertToken src & pushStack tokPos LytProperty
        state' ->
          state' & insertToken src

    -- TokDot tokens usually entail property access, which need masking so we
    -- can use unquoted keywords as labels.
    TokDot ->
      case state & insertDefault of
        ((_, LytForall) : stk', acc') ->
          (stk', acc')
        state' ->
          state' & pushStack tokPos LytProperty

    TokLeftParen ->
      state & insertDefault & pushStack tokPos LytParen

    TokLeftBrace ->
      state & insertDefault & pushStack tokPos LytBrace & pushStack tokPos LytProperty

    TokLeftSquare ->
      state & insertDefault & pushStack tokPos LytSquare

    TokRightParen ->
      state & collapse indentedP & popStack (== LytParen) & insertToken src

    TokRightBrace ->
      state & collapse indentedP & popStack (== LytProperty) & popStack (== LytBrace) & insertToken src

    TokRightSquare ->
      state & collapse indentedP & popStack (== LytSquare) & insertToken src

    TokString _ _ ->
      state & insertDefault & popStack (== LytProperty)

    TokLowerName [] _ ->
      state & insertDefault & popStack (== LytProperty)

    TokOperator _ _ ->
      state & collapse offsideEndP & insertSep & insertToken src

    _ ->
      state & insertDefault

  insertDefault state =
    state & collapse offsideP & insertSep & insertToken src

  insertStart lyt state@(stk, _) =
    -- We only insert a new layout start when it's going to increase indentation.
    -- This prevents things like the following from parsing:
    --     instance foo :: Foo where
    --     foo = 42
    case find (isIndented . snd) stk of
      Just (pos, _) | srcColumn nextPos <= srcColumn pos -> state
      _ -> state & pushStack nextPos lyt & insertToken (lytToken nextPos TokLayoutStart)

  insertSep state@(stk, acc) = case stk of
    -- LytTopDecl is closed by a separator.
    (lytPos, LytTopDecl) : stk' | sepP lytPos ->
      (stk', acc) & insertToken sepTok
    -- LytTopDeclHead can be closed by a separator if there is no `where`.
    (lytPos, LytTopDeclHead) : stk' | sepP lytPos ->
      (stk', acc) & insertToken sepTok
    (lytPos, lyt) : _ | indentSepP lytPos lyt ->
      case lyt of
        -- If a separator is inserted in a case block, we need to push an
        -- additional LytCaseBinders context for comma masking.
        LytOf -> state & insertToken sepTok & pushStack tokPos LytCaseBinders
        _     -> state & insertToken sepTok
    _ -> state
    where
    sepTok = lytToken tokPos TokLayoutSep

  insertKwProperty k state =
    case state & insertDefault of
      ((_, LytProperty) : stk', acc') ->
        (stk', acc')
      state' ->
        k state'

  insertEnd =
    insertToken (lytToken tokPos TokLayoutEnd)

  insertToken token (stk, acc) =
    (stk, acc `snoc` token)

  pushStack lytPos lyt (stk, acc) =
    ((lytPos, lyt) : stk, acc)

  popStack p ((_, lyt) : stk', acc)
    | p lyt = (stk', acc)
  popStack _ state = state

  collapse p = uncurry go
    where
    go ((lytPos, lyt) : stk) acc
      | p lytPos lyt =
          go stk $ if isIndented lyt
                   then acc `snoc` lytToken tokPos TokLayoutEnd
                   else acc
    go stk acc = (stk, acc)

  indentedP =
    const isIndented

  offsideP lytPos lyt =
    isIndented lyt && srcColumn tokPos < srcColumn lytPos

  offsideEndP lytPos lyt =
    isIndented lyt && srcColumn tokPos <= srcColumn lytPos

  indentSepP lytPos lyt =
    isIndented lyt && sepP lytPos

  sepP lytPos =
    srcColumn tokPos == srcColumn lytPos && srcLine tokPos /= srcLine lytPos

unwindLayout :: SourcePos -> [Comment LineFeed] -> LayoutStack -> [SourceToken]
unwindLayout pos leading = go
  where
  go [] = []
  go ((_, LytRoot) : _) = [SourceToken (TokenAnn (SourceRange pos pos) leading []) TokEof]
  go ((_, lyt) : stk) | isIndented lyt = lytToken pos TokLayoutEnd : go stk
  go (_ : stk) = go stk
