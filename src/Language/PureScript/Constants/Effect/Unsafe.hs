module Language.PureScript.Constants.Effect.Unsafe where

import Data.String (IsString)

effectUnsafe :: forall a. (IsString a) => a
effectUnsafe = "Effect_Unsafe"

unsafePerformEffect :: forall a. (IsString a) => a
unsafePerformEffect = "unsafePerformEffect"
