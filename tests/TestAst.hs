{-# LANGUAGE TypeApplications #-}
module TestAst where

import Protolude hiding (Constraint, Type, (:+))

import Control.Lens ((+~))
import Control.Newtype (ala')
import Generic.Random (genericArbitraryRecG, genericArbitraryUG, listOf', uniform, withBaseCase, (:+)(..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary(..), Gen, Property, Testable, counterexample, forAllShrink, subterms, (===))

import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, OpName(..), OpNameType(..), ProperName(..), ProperNameType(..), Qualified(..))
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (Constraint, ConstraintData, SkolemScope(..), Type(..), TypeVarVisibility(..), WildcardData, annForType, everythingOnTypes, everythingWithContextOnTypes, everywhereOnTypes, everywhereOnTypesM, everywhereOnTypesTopDownM, getAnnForType)

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
      all (== 1) `isSatisfiedBy` everywhereOnTypesUnderTest (annForType +~ 1) t

everythingOnTypesSpec :: (([Int] -> [Int] -> [Int]) -> (Type Int -> [Int]) -> Type Int -> [Int]) -> Spec
everythingOnTypesSpec everythingOnTypesUnderTest = do
  it "should visit each type once" $
    forAllShrink (genTypeAnnotatedWith (pure 1) (pure 0)) subterms $ \t ->
      everythingOnTypesUnderTest (++) (pure . getAnnForType) t ===
        filter (== 1) (toList t)


infixr 0 `isSatisfiedBy`
isSatisfiedBy :: forall a p. Show a => Testable p => (a -> p) -> a -> Property
isSatisfiedBy = liftA2 counterexample show

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
    :+ genWildcardData
    :+ genVisibility

  genConstraint :: Gen (Constraint a)
  genConstraint = genericArbitraryUG (genConstraintAnn :+ generatorEnvironment)

  genConstraintData :: Gen ConstraintData
  genConstraintData = genericArbitraryUG generatorEnvironment

  genQualified :: forall b. (Text -> b) -> Gen (Qualified b)
  genQualified ctor = Qualified ByNullSourcePos . ctor <$> genText

  genSkolemScope :: Gen SkolemScope
  genSkolemScope = SkolemScope <$> arbitrary

  genType :: Gen (Type a)
  genType = genericArbitraryRecG (genTypeAnn :+ generatorEnvironment) uniform `withBaseCase` (TypeVar <$> genTypeAnn <*> genText)

  genWildcardData :: Gen WildcardData
  genWildcardData = genericArbitraryUG genText

  maybeOf :: forall b. Gen b -> Gen (Maybe b)
  maybeOf = genericArbitraryUG

  genText :: Gen Text
  genText = pure "x" -- Feel free to make this random if it matters at some point.

  genPSString :: Gen PSString
  genPSString = pure "x" -- Ditto.

  genVisibility :: Gen TypeVarVisibility
  genVisibility = pure TypeVarInvisible
