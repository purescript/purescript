module CompletionSpec where

exampleValue :: Int
exampleValue = 42

exampleFunction :: Int -> Int
exampleFunction _ = 1

exampleInferredString = ""

infixl 5 exampleFunction as \°/ 

data ExampleTypeConstructor a b = ExampleTypeConstructor a b

infixl 5 type ExampleTypeConstructor as \°/ 

class ExampleClass where
  exampleMember :: Int
