module Main where

-- Temporary file that should fail with a yet-to-be-defined error.
-- However, this file currently compiles.
-- The below partially-generated names will both be
--   $$Foo$x$
--
-- When remapping the names from the partially-generated ones to
-- the fully-generated ones, both instances' underlying
-- `TypeIntanceDeclaration`' `chainId` will end up referring
-- to either the first or second instance's identifier
class Foo a
instance Foo x
else instance Foo x
