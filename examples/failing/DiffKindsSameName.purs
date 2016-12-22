-- @shouldFailWith KindsDoNotUnify
module DiffKindsSameName where

import DiffKindsSameName.LibA as LibA
import DiffKindsSameName.LibB as LibB

-- both `LibA` and `LibB` define a kind locally called `DemoKind`
-- `LibB` defines `DemoData :: LibB.DemoKind`
-- if we try to use `DemoData` in a place where `LibA.DemoKind` is expected, it should fail with `KindsDoNotUnify`

data AProxy (m :: LibA.DemoKind) = AProxy

bProxy :: AProxy LibB.DemoData
bProxy = AProxy

