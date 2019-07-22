-- This module only exists so that we perform a full build for the
-- SelfImport.purs module. If this module didn't exist, we would perform a
-- single-module fast rebuild, which doesn't perform the `sortModules` step,
-- and so the error we want to see wouldn't be emitted.
module Dummy where
