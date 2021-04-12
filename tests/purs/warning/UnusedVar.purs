-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
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