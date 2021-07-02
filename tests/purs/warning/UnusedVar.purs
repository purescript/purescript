-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith ShadowedName
module Main where

data X = X


unusedInLambda :: X
unusedInLambda = (\lambdaUnused -> X) X

unusedLetName :: X
unusedLetName =
  let letUnused = X in
  X

unusedWhereIsLet :: X
unusedWhereIsLet =
  X
  where whereUnused = X

unusedLetArgument :: X
unusedLetArgument = 
  let f x letArgUnused = x
  in f X X

notUnusedLet :: X
notUnusedLet =
  let f x = f' x
      f' x = f x
  in
  f X


unusedCaseBinder :: X
unusedCaseBinder = 
  case X of
    caseUnused -> X

unusedObjUpdate :: { foo :: X }
unusedObjUpdate = 
  let x = X
      obj = { foo: X }
  in
  obj { foo = x }

-- The outer x is used in the let-bound expression, the let-binding variable is used in the body
notUnusedNonRecursiveBinding :: X -> X
notUnusedNonRecursiveBinding x = 
  let {x} = {x}
  in x

-- Almost like above but the outer x is not used, as x is bound recursively (Can also be true if there are no 
-- arguments to x but in most cases this will error due to being cyclic)
unusedShadowedByRecursiveBinding :: X -> X
unusedShadowedByRecursiveBinding x = 
  let x _ = x X
  in x X

-- In this case the outer x is used but the new x binding is not
unusedShadowingLet :: X -> X
unusedShadowingLet x = 
  let (x) = x
  in X

-- 4110
oops ∷ { inner :: String } → String
oops box =
  let
    { inner } = box
    val = inner
  in
    val

-- like oops but switching order to show we don't 
notOops ∷ { x :: String } -> String → String
notOops box x =
  let
    val = x
    _blah = x
    { x } = box
  in
    val

bindingGroupsNotRecognised :: Int
bindingGroupsNotRecognised =
  let
    f n = g n
    g n = f n
    
    -- Second f is unused because this is multiple recursive binding groups, we don't warn because we assume
    -- it might be one binding group so there is a usage. If it would be 1 binding group there would be an error
    -- Shadowed variable warnings are similarly not aware of binding groups
    { x } = { x: 2 }
    h n = n
    f x = x
  in 
    h x