-- @shouldFailWith QuantificationCheckFailureInType
module Main where

data Proxy a = Proxy

data P = P (forall a. Proxy a)
