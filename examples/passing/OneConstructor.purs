module OneConstructor where

data One a = One a

one (One a) = a
