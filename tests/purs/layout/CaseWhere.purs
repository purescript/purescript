module Test where

test = case foo of
  Nothing -> a
    where a = 12
  Just a -> do
    what
  where
    foo = bar

test = case f of Foo -> do that
                        where foo = 12
