-- @shouldFailWith KindsDoNotUnify
module DataBindingGroupErrorPos where

-- This isn't really about KindsDoNotUnify, it's about positioning errors
-- that occur in data binding groups

data Foo a = Foo (Bar a a)
data Bar a = Bar (Foo a)
