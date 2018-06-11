-- @shouldFailWith ExportConflict
module C (module A, module B) where

import A as A
import B as B
