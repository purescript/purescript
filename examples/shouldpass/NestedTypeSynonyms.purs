module Test where

  type X = {}
  type Y = X -> X

  fn :: Y
  fn a = a

  test = fn {}
