module SourceFileSpec where

sfValue = "sfValue"

type SFType = String

data SFData = SFOne | SFTwo | SFThree

class SFClass a where
  sfShow :: a -> String
