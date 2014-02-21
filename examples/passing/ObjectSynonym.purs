module ObjectSynonym where

type Inner = Number

inner :: Inner
inner = 0

type Outer = { inner :: Inner }

outer :: Outer
outer = { inner: inner }

module Main where

main = Debug.Trace.trace "Done"  
