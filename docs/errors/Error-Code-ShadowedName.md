This warning occurs when a name was already used earlier. Consider renaming the second occurrence of the name, since multiple uses might make it easy to refer to the wrong value.

For example:

```purescript
circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

-- | The first declaration of the name "picture"
picture :: Picture
picture = [circle, rectangle]


-- | The warning appears here:
area :: Shape -> Number
area s = case s of
  Circle _ r      -> Math.pi * r * r
  Rectangle _ w h -> w * h
  Clipped picture -> boundsArea $ bounds picture
  otherwise       -> 0.0
  where
    boundsArea (Bounds bs) = (bs.right - bs.left) * (bs.bottom - bs.top)

```

Note that build is success in the above case.