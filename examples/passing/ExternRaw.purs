module ExternRaw where

foreign import concat "function concat(xs) { \
                      \  return function(ys) { \
                      \    return xs.concat(ys); \
                      \  };\
                      \}" :: forall a. [a] -> [a] -> [a]
