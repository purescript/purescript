-- |
-- The purpose of this test is to make sure that the empty type class
-- dictionary elimination code doesn't change semantics.
module Main where

import Prelude
import Effect.Console (log)

-- |
-- Data type to check that the result of expressions with eliminated
-- dictionaries are as expected.
data Check = Check
derive instance eqCheck :: Eq Check

-- |
-- This type class has no constraints and no members.
-- Is is therefore considered empty.
class EmptyClass
instance emptyDictInst :: EmptyClass

-- |
-- This type class is not empty as it has members, but it has an empty super
-- class.
class EmptyClass <= HasEmptySuper where
  hasEmptySuper :: Check
instance hasEmptySuperInst :: HasEmptySuper where
  hasEmptySuper = Check

-- |
-- This type class has no members, but has a non-empty super class.
-- It is therefore not empty.
class HasEmptySuper <= HasNonEmptySuper
instance hasNonEmptySuperInst :: HasEmptySuper => HasNonEmptySuper

-- |
-- This type class is empty because all it's super classes are empty and it
-- has no members.
class EmptyClass <= AliasEmptyClass
instance aliasEmptyClassInst :: AliasEmptyClass

whenEmpty :: Check
whenEmpty = Check :: EmptyClass => Check

whenHasEmptySuper :: Check
whenHasEmptySuper = Check :: HasEmptySuper => Check

whenHasNonEmptySuper :: Check
whenHasNonEmptySuper = Check :: HasNonEmptySuper => Check

whenAliasEmptyClass :: Check
whenAliasEmptyClass = Check :: AliasEmptyClass => Check

class WithArgEmpty t
instance withArgEmptyCheck :: WithArgEmpty Check
class WithArgEmpty t <= WithArgHasEmptySuper t where
  withArgHasEmptySuper :: t
instance withArgHasEmptySuperCheck :: WithArgHasEmptySuper Check where
  withArgHasEmptySuper = Check

whenAccessingSuperDict :: Check
whenAccessingSuperDict = foo Check where

  bar :: forall t . WithArgEmpty t => t -> t
  bar x = x

  foo :: forall t . WithArgHasEmptySuper t => t -> t
  foo x = bar x

main =
  if Check == whenEmpty &&
     Check == whenHasEmptySuper &&
     Check == whenHasNonEmptySuper &&
     Check == whenAliasEmptyClass &&
     Check == whenAccessingSuperDict
    then log "Done"
    else pure unit

