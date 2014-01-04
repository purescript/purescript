module MutRec where 

  f 0 = 0
  f x = g x + 1

  g x = f (x / 2)

  data Even = Zero | Even Odd

  data Odd = Odd Even

  evenToNumber :: Even -> Number
  evenToNumber Zero = 0
  evenToNumber (Even n) = oddToNumber n + 1

  oddToNumber :: Odd -> Number
  oddToNumber (Odd n) = evenToNumber n + 1
