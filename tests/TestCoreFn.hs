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
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.CoreFn.ToJSON
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.PSString

import Test.Hspec

main :: IO ()
main = hspec spec

modToJSON :: Version -> Module a -> Value
modToJSON v m = object [ moduleNameToText (moduleName m) .= moduleToJSON v m ] 
  where
  moduleNameToText :: ModuleName -> Text
  moduleNameToText (ModuleName ps) = T.intercalate "." (map runProperName ps)

parseModule :: Value -> Result (Version, ModuleT () ())
parseModule = parse moduleFromJSON

parseMod :: Module a -> Result (ModuleT () ())
parseMod m =
  let v = Version [0] []
  in snd <$> parseModule (modToJSON v m)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

spec :: Spec
spec = context "CoreFnFromJsonTest" $ do
  let mn = ModuleName [ProperName "Example", ProperName "Main"]
  specify "should parse an empty module" $ do
    let r = parseMod $ Module [] mn [] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleName m `shouldBe` mn

  specify "should parse imports" $ do
    let r = parseMod $ Module [] mn [((), mn)] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleImports m `shouldBe` [((), mn)]

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
              [ NonRec () (Ident "x1") $ Literal () (NumericLiteral (Left 1))
              , NonRec () (Ident "x2") $ Literal () (NumericLiteral (Right 1.0))
              , NonRec () (Ident "x3") $ Literal () (StringLiteral (mkString "abc"))
              , NonRec () (Ident "x4") $ Literal () (CharLiteral 'c')
              , NonRec () (Ident "x5") $ Literal () (BooleanLiteral True)
              , NonRec () (Ident "x6") $ Literal () (ArrayLiteral [Literal () (CharLiteral 'a')])
              , NonRec () (Ident "x7") $ Literal () (ObjectLiteral [(mkString "a", Literal () (CharLiteral 'a'))])
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Constructor" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "constructor") $ Constructor () (ProperName "Either") (ProperName "Left") [Ident "value0"] ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Accessor" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "x") $
                  Accessor () (mkString "field") (Literal () $ ObjectLiteral [(mkString "field", Literal () (NumericLiteral (Left 1)))]) ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse ObjectUpdate" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "objectUpdate") $
                  ObjectUpdate ()
                    (Literal () $ ObjectLiteral [(mkString "field", Literal () (StringLiteral (mkString "abc")))])
                    [(mkString "field", Literal () (StringLiteral (mkString "xyz")))]
              ]
    parseMod m `shouldSatisfy` isSuccess
                  
  specify "should parse Abs" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "abs")
                  $ Abs () (Ident "x") (Var () (Qualified Nothing (Ident "x")))
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse App" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "app") 
                  $ App ()
                      (Abs () (Ident "x") (Var () (Qualified Nothing (Ident "x"))))
                      (Literal () (CharLiteral 'c'))
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Case" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "case") $
                  Case () [Var () (Qualified Nothing (Ident "x"))]
                    [ CaseAlternative 
                      [ LiteralBinder () (BooleanLiteral True)
                      , ConstructorBinder () (Qualified (Just (ModuleName [ProperName "Data", ProperName "Either"])) (ProperName "Either")) (Qualified Nothing (ProperName "Left")) [VarBinder () (Ident "z")]
                      , NamedBinder () (Ident "w") (NullBinder ())
                      ]
                      (Left [(Literal () (BooleanLiteral True), Literal () (CharLiteral 'a'))])
                    ]
              ]
    parseMod m `shouldSatisfy` isSuccess

  specify "should parse Let" $ do
    let m = Module [] mn [] [] []
              [ NonRec () (Ident "case") $
                  Let ()
                    [ Rec [(((), Ident "a"), Var () (Qualified Nothing (Ident "x")))] ]
                    (Literal () (BooleanLiteral True))
              ]
    parseMod m `shouldSatisfy` isSuccess
