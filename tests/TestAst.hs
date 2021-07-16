{-# LANGUAGE TypeApplications #-}
module TestAst where

import Protolude hiding (Constraint, Type, (:+))

import Control.Newtype (ala, ala')
import Generic.Random
import Lens.Micro ((+~))
import Test.Hspec
import Test.QuickCheck

import Language.PureScript.Label
import Language.PureScript.Names
import Language.PureScript.PSString
import Language.PureScript.Types

spec :: Spec
spec = do
  describe "Language.PureScript.Types" $ do
    describe "everywhereOnTypes" $ do
      everywhereOnTypesSpec everywhereOnTypes
    describe "everywhereOnTypesM" $ do
      everywhereOnTypesSpec $ ala' Identity everywhereOnTypesM
    describe "everywhereOnTypesTopDownM" $ do
      everywhereOnTypesSpec $ ala' Identity everywhereOnTypesTopDownM
    describe "everythingOnTypes" $ do
      everythingOnTypesSpec everythingOnTypes
    describe "everythingWithContextOnTypes" $ do
      everythingOnTypesSpec $ \f g -> everythingWithContextOnTypes () [] f $ \s -> (s, ) . g

everywhereOnTypesSpec :: ((Type Int -> Type Int) -> Type Int -> Type Int) -> Spec
everywhereOnTypesSpec everywhereOnTypesUnderTest = do
  it "should visit each type once" $
    forAllShrink (genTypeAnnotatedWith (pure 0) (pure 1)) subterms $ \t ->
      let t' = everywhereOnTypesUnderTest (annForType +~ 1) t
      in counterexample (show t') $
        ala' All foldMap (== 1) t'

everythingOnTypesSpec :: (([Int] -> [Int] -> [Int]) -> (Type Int -> [Int]) -> Type Int -> [Int]) -> Spec
everythingOnTypesSpec everythingOnTypesUnderTest = do
  it "should visit each type once" $
    forAllShrink (genTypeAnnotatedWith (pure 1) (pure 0)) subterms $ \t ->
      everythingOnTypesUnderTest (++) (pure . getAnnForType) t ===
        replicate (ala Sum foldMap t) 1


genTypeAnnotatedWith :: forall a. Gen a -> Gen a -> Gen (Type a)
genTypeAnnotatedWith genTypeAnn genConstraintAnn = genType where
  generatorEnvironment
    =  genConstraint
    :+ maybeOf genConstraintData
    :+ Label <$> genPSString
    :+ genPSString
    :+ genQualified (OpName @'TypeOpName)
    :+ genQualified (ProperName @'ClassName)
    :+ genQualified (ProperName @'TypeName)
    :+ genSkolemScope
    :+ maybeOf genSkolemScope
    :+ genText
    :+ listOf' (listOf' genText)
    :+ maybeOf genText
    :+ genType
    :+ listOf' genType
    :+ maybeOf genType

  genConstraint :: Gen (Constraint a)
  genConstraint = genericArbitraryUG (genConstraintAnn :+ generatorEnvironment)

  genConstraintData :: Gen ConstraintData
  genConstraintData = genericArbitraryUG generatorEnvironment

  genQualified :: forall b. (Text -> b) -> Gen (Qualified b)
  genQualified ctor = Qualified Nothing . ctor <$> genText

  genType :: Gen (Type a)
  genType = genericArbitraryRecG (genTypeAnn :+ generatorEnvironment) uniform `withBaseCase` (TypeVar <$> genTypeAnn <*> genText)

  genSkolemScope :: Gen SkolemScope
  genSkolemScope = SkolemScope <$> arbitrary

  maybeOf :: forall b. Gen b -> Gen (Maybe b)
  maybeOf genJust = oneof [pure Nothing, Just <$> genJust]

  genText :: Gen Text
  genText = pure "x" -- Feel free to make this random if it matters at some point.

  genPSString :: Gen PSString
  genPSString = pure "x" -- Ditto.
