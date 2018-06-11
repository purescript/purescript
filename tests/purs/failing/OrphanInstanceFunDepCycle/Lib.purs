module Lib where
-- covering sets: {{l}, {r}}
class C l r | l -> r, r -> l
data R
