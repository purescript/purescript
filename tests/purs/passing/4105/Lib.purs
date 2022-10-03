module Lib where

type Template col = { bio :: col String }
type Identity a = a
type Patch = Template Identity
