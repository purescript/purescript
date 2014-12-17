module Data.Function where

on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = g x `f` g y

foreign import data Fn0 :: * -> *
foreign import data Fn1 :: * -> * -> *
foreign import data Fn2 :: * -> * -> * -> *
foreign import data Fn3 :: * -> * -> * -> * -> *
foreign import data Fn4 :: * -> * -> * -> * -> * -> *
foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *
foreign import data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

foreign import mkFn0
  "function mkFn0(fn) {\
  \  return function() {\
  \    return fn({});\
  \  };\
  \}" :: forall a. (Unit -> a) -> Fn0 a

foreign import mkFn1
  "function mkFn1(fn) {\
  \  return function(a) {\
  \    return fn(a);\
  \  };\
  \}" :: forall a b. (a -> b) -> Fn1 a b

foreign import mkFn2
  "function mkFn2(fn) {\
  \  return function(a, b) {\
  \    return fn(a)(b);\
  \  };\
  \}" :: forall a b c. (a -> b -> c) -> Fn2 a b c

foreign import mkFn3
  "function mkFn3(fn) {\
  \  return function(a, b, c) {\
  \    return fn(a)(b)(c);\
  \  };\
  \}" :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

foreign import mkFn4
  "function mkFn4(fn) {\
  \  return function(a, b, c, d) {\
  \    return fn(a)(b)(c)(d);\
  \  };\
  \}" :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

foreign import mkFn5
  "function mkFn5(fn) {\
  \  return function(a, b, c, d, e) {\
  \    return fn(a)(b)(c)(d)(e);\
  \  };\
  \}" :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

foreign import mkFn6
  "function mkFn6(fn) {\
  \  return function(a, b, c, d, e, f) {\
  \    return fn(a)(b)(c)(d)(e)(f);\
  \  };\
  \}" :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

foreign import mkFn7
  "function mkFn7(fn) {\
  \  return function(a, b, c, d, e, f, g) {\
  \    return fn(a)(b)(c)(d)(e)(f)(g);\
  \  };\
  \}" :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

foreign import mkFn8
  "function mkFn8(fn) {\
  \  return function(a, b, c, d, e, f, g, h) {\
  \    return fn(a)(b)(c)(d)(e)(f)(g)(h);\
  \  };\
  \}" :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

foreign import mkFn9
  "function mkFn9(fn) {\
  \  return function(a, b, c, d, e, f, g, h, i) {\
  \    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);\
  \  };\
  \}" :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

foreign import mkFn10
  "function mkFn10(fn) {\
  \  return function(a, b, c, d, e, f, g, h, i, j) {\
  \    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);\
  \  };\
  \}" :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

foreign import runFn0
  "function runFn0(fn) {\
  \  return fn();\
  \}" :: forall a. Fn0 a -> a

foreign import runFn1
  "function runFn1(fn) {\
  \  return function(a) {\
  \    return fn(a);\
  \  };\
  \}" :: forall a b. Fn1 a b -> a -> b

foreign import runFn2
  "function runFn2(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return fn(a, b);\
  \    };\
  \  };\
  \}" :: forall a b c. Fn2 a b c -> a -> b -> c

foreign import runFn3
  "function runFn3(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return fn(a, b, c);\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

foreign import runFn4
  "function runFn4(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return fn(a, b, c, d);\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

foreign import runFn5
  "function runFn5(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return fn(a, b, c, d, e);\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f

foreign import runFn6
  "function runFn6(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return function(f) {\
  \              return fn(a, b, c, d, e, f);\
  \            };\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g

foreign import runFn7
  "function runFn7(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return function(f) {\
  \              return function(g) {\
  \                return fn(a, b, c, d, e, f, g);\
  \              };\
  \            };\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h

foreign import runFn8
  "function runFn8(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return function(f) {\
  \              return function(g) {\
  \                return function(h) {\
  \                  return fn(a, b, c, d, e, f, g, h);\
  \                };\
  \              };\
  \            };\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i

foreign import runFn9
  "function runFn9(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return function(f) {\
  \              return function(g) {\
  \                return function(h) {\
  \                  return function(i) {\
  \                    return fn(a, b, c, d, e, f, g, h, i);\
  \                  };\
  \                };\
  \              };\
  \            };\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

foreign import runFn10
  "function runFn10(fn) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return function(f) {\
  \              return function(g) {\
  \                return function(h) {\
  \                  return function(i) {\
  \                    return function(j) {\
  \                      return fn(a, b, c, d, e, f, g, h, i, j);\
  \                    };\
  \                  };\
  \                };\
  \              };\
  \            };\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k