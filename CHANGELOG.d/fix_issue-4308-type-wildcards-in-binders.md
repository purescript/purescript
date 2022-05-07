* Do not emit warnings about type wildcards used in binders (patterns).

  Type wildcards in the following examples no longer trigger a warning:

  ```
  f :: Int
  f = 42 # \(x :: _) -> x

  g :: Maybe Int
  g = do
    x :: _ <- getX
    pure $ x + 5
  ```
