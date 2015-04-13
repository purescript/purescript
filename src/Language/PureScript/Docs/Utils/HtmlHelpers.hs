
module Language.PureScript.Docs.Utils.HtmlHelpers where

import Data.String
import Lucid

text :: String -> Html ()
text = toHtml

sp :: Html ()
sp = text " "

para :: String -> Html () -> Html ()
para className content = p_ [class_ (fromString className)] content

withClass :: String -> Html () -> Html ()
withClass className content = span_ [class_ (fromString className)] content

linkTo :: String -> Html () -> Html ()
linkTo href inner = a_ [href_ (fromString href)] inner
