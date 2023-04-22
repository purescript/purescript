{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various constants which refer to things in Prim
module Language.PureScript.Constants.Prim where

import Language.PureScript.Names (ModuleName)
import Language.PureScript.Constants.TH qualified as TH

$(TH.declare do
  TH.mod "Prim" do
    TH.cls "Partial"
    TH.ty "Array"
    TH.ty "Boolean"
    TH.ty "Char"
    TH.ty "Constraint"
    TH.ty "Function"
    TH.ty "Int"
    TH.ty "Number"
    TH.ty "Record"
    TH.ty "Row"
    TH.ty "String"
    TH.ty "Symbol"
    TH.ty "Type"
    TH.asIdent do TH.asString do TH.var "undefined"

  TH.mod "Prim.Boolean" do
    TH.tys ["False", "True"]

  TH.mod "Prim.Coerce" do
    TH.cls "Coercible"

  TH.mod "Prim.Int" do
    TH.prefixWith "Int" do TH.clss ["Add", "Compare", "Mul", "ToString"]

  TH.mod "Prim.Ordering" do
    TH.prefixWith "Type" do TH.ty "Ordering"
    TH.tys ["EQ", "GT", "LT"]

  TH.mod "Prim.Row" do
    TH.prefixWith "Row" do TH.clss ["Cons", "Lacks", "Nub", "Union"]
  
  TH.mod "Prim.RowList" do
    TH.ty "RowList"
    TH.cls "RowToList"
    TH.prefixWith "RowList" do TH.tys ["Cons", "Nil"]

  TH.mod "Prim.Symbol" do
    TH.prefixWith "Symbol" do TH.clss ["Append", "Compare", "Cons"]

  TH.mod "Prim.TypeError" do
    TH.clss ["Fail", "Warn"]
    TH.tys ["Above", "Beside", "Doc", "Quote", "QuoteLabel", "Text"]

  )

primModules :: [ModuleName]
primModules = [M_Prim, M_Prim_Boolean, M_Prim_Coerce, M_Prim_Ordering, M_Prim_Row, M_Prim_RowList, M_Prim_Symbol, M_Prim_Int, M_Prim_TypeError]
