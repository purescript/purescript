module A where

import B as Main

-- Prior to the 2018 fix this would be detected as a cycle between A and Main.
foo ∷ Main.Foo → Main.Foo
foo x = x
