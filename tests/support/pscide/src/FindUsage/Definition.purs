module FindUsage.Definition (Usage(..), ($%), usageId, toBeReexported) where

data Usage
  = Used Int
  | Usage Int Int

infixl 2 Usage as $%

usageId ∷ ∀ a. a → a
usageId x = x

toBeReexported ∷ ∀ a. a → a
toBeReexported = usageId
