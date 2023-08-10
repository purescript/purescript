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
  Expected type
    Y
  but found type
    X
  ```

  **Case 2: reporting all label differences (and only those differences) together**
  
  Before:
  ```
  Type of expression contains additional label extraLabel1
  ```
  (without any messages about more extra labels)
  
  or
  
  ```
  Type of expression lacks required label requiredLabel1
  ```
  (without any messages about more required labels)
  
  or
  
  ```
  Could not match type
    ( a :: Int
    , b :: Int
    , c :: Int
    , d :: Int
    , e :: Int
    , f :: Int
    , extraLabel1 :: Int
    , extraLabel2 :: Int
    ...
    )
  with type
    ( a :: t7
    , b :: t6
    , c :: t5
    , d :: t4
    , e :: t3
    , f :: t2
    , requiredLabel1 :: t1
    ...
    | t8
    )
  ```
  
  After:
  ```
  Expected type
    { requiredLabel1 :: t1
    ...
    | t2
    }
  but found type
    { extraLabel1 :: Int
    , extraLabel2 :: Int
    ...
    }
  ```

  **Case 3: reporting only the first `TypesDoNotUnify` error rather than all such errors on record pattern matches whose labels are a subset of the expected record's labels**

  The following behavior is unchanged, but it is explained to clarify
  the next summary.
 
  When pattern matching on a record where the pattern match's labels
  differ from the labels of the expected record (e.g. from a type signature),
  a `TypesDoNotUnify` error is thrown showing a diff between the two records' labels,
  regardless of whether their corresponding types unify.

  ```purs
  data ExpectedType = ExpectedValue
  data SomeOtherType = DifferentValue

  {-  
  Produces one error for the entire record; behavior unchanged.

    The type
      ( a :: ExpectedValue
      , b :: String
      )
    does not unify with type
      ( x :: SomeOtherType
      , y :: Int
      )
  -}
  test :: { a :: ExpectedType, b :: String } -> Int
  test { x: DifferentValue, y: 7 } = 1
  ```

  However, if the labels in the pattern match are a subset of the labels
  in the expected record, then previously a `TypesDoNotUnify` error is thrown
  for only the _first_ detected error. Now, such an error is thrown
  _for each_ type that fails to unify.

  ```purs
  data ExpectedType = ExpectedValue
  data SomeOtherType = DifferentValue

  data A = A

  -- Errors thrown in previous behavior:
  -- - under label sameLabel1, `TypesDoNotUnify ExpectedType SomeOtherType`
  --
  -- Errors thrown in new behavior:
  -- - under label sameLabel1, `TypesDoNotUnify ExpectedType SomeOtherType`
  -- - under label sameLabel2, `TypesDoNotUnify String Int`
  test :: { sameLabel1 :: ExpectedType, sameLabel2 :: String, sameLabel3 :: A } -> Int
  test { sameLabel1: DifferentValue, sameLabel2: 7, sameLabel3: A } = 1
  ```

  **Case 4: printing `Record row` using native syntax**

  | Before | After |
  | - | - |
  | `Record ()` | `{}` |
  | `Record (...)` | `{ ... }` |
