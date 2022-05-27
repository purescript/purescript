-- |
-- ## High-Level Summary
--
-- This section provides a high-level summary of this file. For those who
-- know more about compiler-development, the below explanation is likely enough.
-- For everyone else, see the next section.
--
-- The parser itself is unaware of indentation, and instead only parses explicit
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
--
-- ## Detailed Summary
--
-- ### The Problem
--
-- The parser itself is unaware of indentation or other such layout concerns.
-- Rather than dealing with it explicitly, the parser and its
-- grammar rules are only aware of normal tokens (e.g. @TokLowerName@) and
-- three special zero-width tokens, @TokLayoutStart@, @TokLayoutSep@,
-- and @TokLayoutEnd@. This is convenient because the actual grammar
-- can be specified apart from the indentation rules and other such
-- layout concerns.
--
-- For a simple example, the parser parses all three examples of the code below
-- using the exact same grammar rules for the @let@ keyword despite
-- each example using different indentations levels:
--
-- @
-- -- Example 1
-- let foo = 5
--     x = 2 in foo
--
-- -- Example 2
-- let
--   bar = 5
--   y = 2
-- in bar
--
-- -- Example 3
-- let        baz
--                  =
--              5
--            z= 2 in baz
-- @
--
-- Each block of code might appear to the parser as a stream of the
-- following source tokens where the @\{@ sequence represents
-- @TokLayoutStart@, the @\;@ sequence represents @TokLayoutSep@,
-- and the @\}@ sequence represents @TokLayoutEnd@:
-- - @let \{foo = 5\;x = 2\} in foo@
-- - @let \{bar = 5\;y = 2\} in bar@
-- - @let \{baz = 5\;z = 2\} in baz@
--
--
-- For a more complex example, consider commas:
--
-- @
-- case one, { twoA, twoB }, [ three1
--    , three2
--    ,   do
--      { three3, three4 } <- case arg1, arg2 of
--         Nothing, _ -> { three3: 1, three4: 2 }
--         Just _, Nothing -> { three3: 2, three4: 3 }
--         _, _ -> { three3: 3, three4: 4 }
--      pure $ three3 + three4
--    ] of
-- @
--
-- Which of the above 13 commas function as the separaters between the
-- case binders (e.g. @one@) in the outermost @case ... of@ context?
--
-- ### The Solution
--
-- The parser doesn't have to care about layout concerns (e.g. indentation
-- or what starts and ends a context, such as a case binder) because the
-- lexer solves that problem instead.
--
-- So, how does the lexer solve this problem? It follows this general algorithm:
-- 1. Lex the source code text into an initial stream of `SourceToken`s
--    that do not have any of the three special tokens mentioned previously.
-- 2. On a token-by-token basis, determine whether the lexer should
--        1. insert one of the three special tokens,
--        2. modify the current context (e.g. are we within a case binder?
--           Are we in a record expression?)
--
-- Step 2 is handled via 'insertLayout' and is essentially a state machine.
-- The layout delimiters, (e.g. 'LytCase', 'LytBrace', 'LytProperty',
-- and 'LytOf' in the next section's example) either stop certain "rules"
-- from applying or ensure that certain "rules" now apply. By "rules",
-- we mean whether and where one of the three special tokens are added.
-- The comments in the source code for the 'insertLayout' algorithm call
-- pushing these delimiters onto the stack "masking" and popping them off
-- as "unmasking". Seeing when a layout delimiter is pushed and popped
-- are the keys to understanding this algorithm.
--
-- ### Walking Through an Example
--
-- Before showing an example, let's remember a few things.
--   1. The @TokLowerName "case"@ token (i.e. a "case" keyword) indicates the start
--      of a @case ... of@ context. That context includes case binders (like the
--      example shown previously) that can get quite complex. When encountered,
--      we may need to insert one or more of the three special tokens here
--      until we encounter the terminating @TokLowerName "of"@ token that
--      signifies its end.
--   2. "case" and "of" can also appear as a record field's name. In such a context,
--      they would not start or end a @case ... of@ block.
--
-- Given the below source code...
--
-- @
-- case { case: "foo", of: "bar" } of
-- @
--
-- the lexer would go through something like the following states:
-- 1. Encountered @TokLowerName "case"@. Update current context to
--    "within a case of expression" by pushing the 'LytCase' delimiter
--    onto the layout delimiter stack. Insert the @case@ token
--    into the stream of source tokens.
-- 2. Encountered @TokLeftBrace@. Update current context to
--    "within a record expression" by pushing the 'LytBrace' delimiter.
--    Since we expect a field name to be the next token we see,
--    which may include a reserved keyword, update the current context again to
--    "expecting a field name" by pushing the `LytProperty`.
--    delimiter. Insert the @{@ token into the stream of source tokens.
-- 3. Encountered @TokLowerName "case"@. Check the current context.
--    Since it's a `LytProperty`, this is a field name and we shouldn't
--    assume that the next few tokens will be case binders. However,
--    since this might be a record with no more fields, update the
--    current context back to "within a record expression" by popping
--    the `LytProperty` off the layout delimiter stack. Insert the @case@ token
-- 4. Encountered @TokColon@. Insert the @:@ token
-- 5. Encountered @TokLowerName "foo"@. Insert the @foo@ token.
-- 6. Encountered @TokComma@. Check the current context. Since it's a `LytBrace`,
--    we're in a record expression and there is another field. Update the
--    current context by pushing `LytProperty` as we expect a field name again.
-- 7. Encountered @TokLowerName "of"@. Check the current context.
--    Since it's a `LytProperty`, this is a field name rather
--    than the end of a case binder. Thus, we don't expect the next tokens
--    to be the @body@ in a @case ... of body@ expression. However, since
--    this might be a record with no more fields, update the current context
--    back to "within a record expression" by popping the `LytProperty`
--    off the stack. Insert the @of@ token.
-- 8. Encountered @TokRightBrace@. Check the current context.
--    Since it's a `LytBrace`, this is the end of a record expression.
--    Update the current context to "within a case of expression"
--    by popping the `LytBrace` off the stack. Insert the @}@ token.
-- 9. Encountered @TokLowername "of"@. Check the current context.
--    Since it's a 'LytCase', this is the end of a @case ... of@ expression
--    and the body will follow. Update the current context to
--    "body of a case of expression" by pushing 'LytOf' onto the layout stack.
--    Insert the @of@ token into the stream of tokens.
--
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
    -- LytDeclGuard or LytCaseGuard context.
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
          state & collapse offsideEndP & insertSep & insertToken src & pushStack tokPos LytTick

    -- In general, commas should close all indented contexts.
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
