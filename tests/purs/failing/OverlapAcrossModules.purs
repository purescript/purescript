-- @shouldFailWith OverlappingInstances
module OverlapAcrossModules where
import OverlapAcrossModules.Class
import OverlapAcrossModules.X
data Y
instance cxy :: C X Y

