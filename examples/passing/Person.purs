module Person where

  data Person = Person { name :: String, age :: Number }

  foreign import itoa :: Number -> String
	  
  showPerson :: Person -> String
  showPerson = \p -> case p of
    Person o -> o.name ++ ", aged " ++ itoa(o.age)
