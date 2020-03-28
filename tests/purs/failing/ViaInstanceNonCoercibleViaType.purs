-- @shouldFailWith NoInstanceFound
module Main where

class C (a :: Type)

data V = V

instance cv :: C V

data D = D

derive via V instance cd :: C D
