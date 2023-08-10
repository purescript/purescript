* Significantly improved `Record`/`Row`-related error messages

  The problems of the prior Record/Row-related error messages
  are shown below with their updated counterparts. Vertical spacing
  in the messages is omitted for brevity.
  
  The errors `ExprDoesNotHaveType`, `PropertyIsMissing`, 
  and `AdditionalProperty` have been removed, being replaced
  with `TypesDoNotUnify` instead.

  **Case 1: lack of expected/actual text when that is known**

  The expected/actual distinction is not possible to determine in all cases,
  but it can be known in some cases. Previously, it was not reported even 
  if it was known. Now the compiler reports this distinction if known.

  Before:
  ```
  Could not match type
    X
  with type
    Y
  ```
  After:
  ```
  The actual type
    X
  does not match the expected type
    Y
  ```

  **Case 2: reporting only the first additional label**

  Before:
  ```
  Type of expression contains additional label extraLabel1
  ```
  After:
  ```
  The actual type
    { extraLabel1 :: Int
    , extraLabel2 :: Int
    }
  does not match the expected type
    { ... }
  ```

  **Case 3: reporting only the first missing label**

  Before:
  ```
  Type of expression lacks required label age
  ```
  After:
  ```
  The actual type
    { first :: String
    , last :: String
    ...
    }
  does not match the expected type
    { age :: Number
    ...
    | t0
    }
  ```

  **Case 4: reporting all of two large records rather than just their differences**

  Before:
  ```
  Could not match type
    ( a :: Int
    , b :: Int
    , c :: Int
    , d :: Int
    , e :: Int
    , f :: Int
    , h :: Int
    , j :: Int
    ...
    )
  with type
    ( a :: t7
    , b :: t6
    , c :: t5
    , d :: t4
    , e :: t3
    , f :: t2
    , g :: t1
    ...
    | t8
    )
  ```
  After:
  ```
  Could not match type
    ( h :: Int
    , j :: Int
    ...
    )
  with type
    ( g :: t1
    ...
    | t8
    )
  ```

  **Case 5: printing `Record ()` using native syntax**

  Before:
  ```
  Record ()
  ```
  After:
  ```
  {}
  ```

  **Case 6: printing `Record (...)` using native syntax**

  Before:
  ```
  Record (...)
  ```
  After:
  ```
  { ... }
  ```
