data Maybe a = Nothing | Just a

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

fmap m f = maybe Nothing (\a -> Just (f a)) m

bind m f = maybe Nothing f m
