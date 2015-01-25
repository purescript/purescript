module Main where

  import Prelude ((++))

  data Person = Person { name :: String, age :: Number }

  foreign import itoa
    """
    function itoa(n) {
      return n.toString();
    }
    """ :: Number -> String

  showPerson :: Person -> String
  showPerson = \p -> case p of
    Person o -> o.name ++ ", aged " ++ itoa(o.age)

  main = Debug.Trace.trace "Done"
