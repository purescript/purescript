{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCoreFn (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Types as Aeson
import Data.Version

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.Comments
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.CoreFn.ToJSON
import Language.PureScript.Names
import Language.PureScript.PSString

import Test.Tasty
import Test.Tasty.Hspec

main :: IO TestTree
main = testSpec "corefn" spec

parseModule :: Value -> Result (Version, Module Ann)
parseModule = parse moduleFromJSON

-- convert a module to its json CoreFn representation and back
parseMod :: Module Ann -> Result (Module Ann)
parseMod m =
  let v = Version [0] []
  in snd <$> parseModule (moduleToJSON v m)

isSuccess :: Result a -> Bool
isSuccess (Aeson.Success _) = True
isSuccess _           = False

spec :: Spec
spec = context "CoreFnFromJsonTest" $ do
  let mn = ModuleName "Example.Main"
      mp = "src/Example/Main.purs"
      ss = SourceSpan mp (SourcePos 0 0) (SourcePos 0 0)
      ann = ssAnn ss

  specify "should parse version" $ do
    let v = Version [0, 13, 6] []
        m = Module ss [] mn mp [] [] [] []
        r = fst <$> parseModule (moduleToJSON v m)
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success v' -> v' `shouldBe` v

  specify "should parse an empty module" $ do
    let r = parseMod $ Module ss [] mn mp [] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success m -> moduleName m `shouldBe` mn

  specify "should parse source span" $ do
    let r = parseMod $ Module ss [] mn mp [] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success m -> moduleSourceSpan m `shouldBe` ss

  specify "should parse module path" $ do
    let r = parseMod $ Module ss [] mn mp [] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success m -> modulePath m `shouldBe` mp

  specify "should parse imports" $ do
    let r = parseMod $ Module ss [] mn mp [(ann, mn)] [] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success m -> moduleImports m `shouldBe` [(ann, mn)]

  specify "should parse exports" $ do
    let r = parseMod $ Module ss [] mn mp [] [Ident "exp"] [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success m -> moduleExports m `shouldBe` [Ident "exp"]

  specify "should parse foreign" $ do
    let r = parseMod $ Module ss [] mn mp [] [] [Ident "exp"] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Aeson.Success m -> moduleForeign m `shouldBe` [Ident "exp"]

  context "Expr" $ do
    specify "should parse literals" $ do
      let m = Module ss [] mn mp [] [] []
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
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "constructor") $ Constructor ann (ProperName "Either") (ProperName "Left") [Ident "value0"] ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Accessor" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "x") $
                    Accessor ann (mkString "field") (Literal ann $ ObjectLiteral [(mkString "field", Literal ann (NumericLiteral (Left 1)))]) ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse ObjectUpdate" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "objectUpdate") $
                    ObjectUpdate ann
                      (Literal ann $ ObjectLiteral [(mkString "field", Literal ann (StringLiteral (mkString "abc")))])
                      [(mkString "field", Literal ann (StringLiteral (mkString "xyz")))]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Abs" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "abs")
                    $ Abs ann (Ident "x") (Var ann (Qualified (Just mn) (Ident "x")))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse App" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "app")
                    $ App ann
                        (Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "x"))))
                        (Literal ann (CharLiteral 'c'))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Case" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified Nothing (Ident "x"))]
                      [ CaseAlternative
                        [ NullBinder ann ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Case with guards" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified Nothing (Ident "x"))]
                      [ CaseAlternative
                        [ NullBinder ann ]
                        (Left [(Literal ann (BooleanLiteral True), Literal ann (CharLiteral 'a'))])
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Let" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "case") $
                    Let ann
                      [ Rec [((ann, Ident "a"), Var ann (Qualified Nothing (Ident "x")))] ]
                      (Literal ann (BooleanLiteral True))
                ]
      parseMod m `shouldSatisfy` isSuccess

  context "Meta" $ do
    specify "should parse IsConstructor" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec (ss, [], Nothing, Just (IsConstructor ProductType [Ident "x"])) (Ident "x") $
                  Literal (ss, [], Nothing, Just (IsConstructor SumType [])) (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsNewtype" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec (ss, [], Nothing, Just IsNewtype) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsTypeClassConstructor" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec (ss, [], Nothing, Just IsTypeClassConstructor) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsForeign" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec (ss, [], Nothing, Just IsForeign) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

  context "Binders" $ do
    specify "should parse LiteralBinder" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified Nothing (Ident "x"))]
                      [ CaseAlternative
                        [ LiteralBinder ann (BooleanLiteral True) ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse VarBinder" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified Nothing (Ident "x"))]
                      [ CaseAlternative
                        [ ConstructorBinder
                            ann
                            (Qualified (Just (ModuleName "Data.Either")) (ProperName "Either"))
                            (Qualified Nothing (ProperName "Left"))
                            [VarBinder ann (Ident "z")]
                        ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse NamedBinder" $ do
      let m = Module ss [] mn mp [] [] []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified Nothing (Ident "x"))]
                      [ CaseAlternative
                        [ NamedBinder ann (Ident "w") (NamedBinder ann (Ident "w'") (VarBinder ann (Ident "w''"))) ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

  context "Comments" $ do
    specify "should parse LineComment" $ do
      let m = Module ss [ LineComment "line" ] mn mp [] [] [] []
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse BlockComment" $ do
      let m = Module ss [ BlockComment "block" ] mn mp [] [] [] []
      parseMod m `shouldSatisfy` isSuccess
