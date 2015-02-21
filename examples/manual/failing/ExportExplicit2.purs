-- should fail as Y is not a data constructor for X
module M1 (X(Y)) where

  data X = X
  data Y = Y
