-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedDeclaration

module Main
  ( unusedArgDecl
  , X(..)
  ) where

data X = X


unusedArgDecl :: X -> X
unusedArgDecl unusedArg = X

unusedDecl :: X
unusedDecl = 
  X