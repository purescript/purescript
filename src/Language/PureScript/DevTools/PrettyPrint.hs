module Language.PureScript.DevTools.PrettyPrint where

import Data.List (intercalate)
import Language.PureScript.Pretty
import Language.PureScript.DevTools.ErrorTypes
import Data.Monoid

prettyPrintErrorStack :: Bool -> ErrorStack -> String
prettyPrintErrorStack printFullStack (ErrorStack es) =
  case mconcat $ map (Last . compileErrorPosition) es of
    Last (Just sourcePos) -> "Error at " ++ show sourcePos ++ ": \n" ++ prettyPrintErrorStack'
    _ -> prettyPrintErrorStack'
  where
  prettyPrintErrorStack' :: String
  prettyPrintErrorStack'
    | printFullStack = intercalate "\n" (map showError (filter isErrorNonEmpty es))
    | otherwise =
      let
        es' = filter isErrorNonEmpty es
      in case length es' of
        1 -> showError (head es')
        _ -> showError (head es') ++ "\n" ++ showError (last es')

showError :: CompileError -> String
showError (CompileError msg Nothing _) = msg
showError (CompileError msg (Just (ValueError val)) _) = "Error in value " ++ prettyPrintValue val ++ ":\n" ++ msg
showError (CompileError msg (Just (TypeError ty)) _) = "Error in type " ++ prettyPrintType ty ++ ":\n" ++ msg

isErrorNonEmpty :: CompileError -> Bool
isErrorNonEmpty = not . null . compileErrorMessage
