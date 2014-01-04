module MethodCall where

  foreign import foo :: () -> {} 

  test = {
      foo();
      return 0;
    }
