module Monad where

  type Monad m = { ret :: forall a. a -> m a
		 , bind :: forall a b. m a -> (a -> m b) -> m b }

  data Id a = Id a

  id :: Monad Id
  id = { ret : Id
       , bind : \ma f -> case ma of Id a -> f a }

  data Maybe a = Nothing | Just a

  maybe :: Monad Maybe
  maybe = { ret : Just
	  , bind : \ma f -> case ma of
	      Nothing -> Nothing
	      Just a -> f a 
	  }

  test :: forall m. Monad m -> m Number
  test = \m -> m.bind (m.ret 1) (\n1 ->
		 m.bind (m.ret "Test") (\n2 -> 
		   m.ret n1))

  test1 = test id

  test2 = test maybe
