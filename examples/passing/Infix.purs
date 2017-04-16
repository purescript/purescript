module Main where

  import Debug.Trace

  data Bool = True | False

  instance boolLikeBool :: BoolLike Bool where
    (&&) True True = True
    (&&) _    _    = False

    (||) False False = False
    (||) _     _     = True

    not True  = False
    not False = True

  instance eqBool :: Eq Bool where
    (==) True  True  = true
    (==) False False = true
    (==) _     _     = false

    (/=) b b' = not (b == b')

  instance showBool :: Show Bool where
    show True  = "True"
    show False = "False"

  main = trace $ "True && False == False: " ++ (show (True && False == False))
