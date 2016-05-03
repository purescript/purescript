-- @shouldFailWith TransitiveExportError
module Test where

import InstanceExport
import Prelude

test = f $ S "Test"
