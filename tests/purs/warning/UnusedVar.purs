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