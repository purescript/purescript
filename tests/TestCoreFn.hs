{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCoreFn (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.CoreFn.ToJSON
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.PSString

import Test.Hspec

main :: IO ()
main = hspec spec

modToJSON :: Version -> Module Ann -> Value
modToJSON v m = object [ moduleNameToText (moduleName m) .= moduleToJSON v m ] 
  where
  moduleNameToText :: ModuleName -> Text
  moduleNameToText (ModuleName ps) = T.intercalate "." (map runProperName ps)

parseModule :: Value -> Result (Version, ModuleT () Ann)
parseModule = parse moduleFromJSON

-- convert a module to its json CoreFn representation and back
parseMod :: Module Ann -> Result (ModuleT () Ann)
parseMod m =
  let v = Version [0] []
  in snd <$> parseModule (modToJSON v m)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

spec :: Spec
spec = context "CoreFnFromJsonTest" $ do
  let mn = ModuleName [ProperName "Example", ProperName "Main"]
      ann = ssAnn (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0))
  specify "should parse an empty module" $ do
    let r = parseMod $ Module [] mn [] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleName m `shouldBe` mn

  specify "should parse imports" $ do
    let r = parseMod $ Module [] mn [(ann, mn)] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleImports m `shouldBe` [(ann, mn)]

  specify "should parse exports" $ do
    let r = parseMod $ Module [] mn [] [Ident "exp"] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleExports m `shouldBe` [Ident "exp"]

  specify "should parse foreign" $ do
    let r = parseMod $ Module [] mn [] [] [(Ident "exp", TUnknown 0)] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleForeign m `shouldBe` [(Ident "exp", ())]

  specify "should parse literals" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "x1") $ Literal ann (NumericLiteral (Left 1))
              , NonRec ann (Ident "x2") $ Literal ann (NumericLiteral (Right 1.0))
              , NonRec ann (Ident "x3") $ Literal ann (StringLiteral (mkString "abc"))
              , NonRec ann (Ident "x4") $ Literal ann (CharLiteral 'c')
              , NonRec ann (Ident "x5") $ Literal ann (BooleanLiteral True)
              , NonRec ann (Ident "x6") $ Literal ann (ArrayLiteral [Literal ann (CharLiteral 'a')])
              , NonRec ann (Ident "x7") $ Literal ann (ObjectLiteral [(mkString "a", Literal ann (CharLiteral 'a'))])
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Constructor" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "constructor") $ Constructor ann (ProperName "Either") (ProperName "Left") [Ident "value0"] ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Accessor" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "x") $
                  Accessor ann (mkString "field") (Literal ann $ ObjectLiteral [(mkString "field", Literal ann (NumericLiteral (Left 1)))]) ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse ObjectUpdate" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "objectUpdate") $
                  ObjectUpdate ann
                    (Literal ann $ ObjectLiteral [(mkString "field", Literal ann (StringLiteral (mkString "abc")))])
                    [(mkString "field", Literal ann (StringLiteral (mkString "xyz")))]
              ]
    parseMod m `shouldSatisfy` isSuccess
                  
  specify "should parse Abs" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "abs")
                  $ Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "x")))
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse App" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "app") 
                  $ App ann
                      (Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "x"))))
                      (Literal ann (CharLiteral 'c'))
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Case" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "case") $
                  Case ann [Var ann (Qualified Nothing (Ident "x"))]
                    [ CaseAlternative 
                      [ LiteralBinder ann (BooleanLiteral True)
                      , ConstructorBinder ann (Qualified (Just (ModuleName [ProperName "Data", ProperName "Either"])) (ProperName "Either")) (Qualified Nothing (ProperName "Left")) [VarBinder ann (Ident "z")]
                      , NamedBinder ann (Ident "w") (NullBinder ann)
                      ]
                      (Left [(Literal ann (BooleanLiteral True), Literal ann (CharLiteral 'a'))])
                    ]
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Let" $ do
    let m = Module [] mn [] [] []
              [ NonRec ann (Ident "case") $
                  Let ann
                    [ Rec [((ann, Ident "a"), Var ann (Qualified Nothing (Ident "x")))] ]
                    (Literal ann (BooleanLiteral True))
              ]
    parseMod m `shouldSatisfy` isSuccess
