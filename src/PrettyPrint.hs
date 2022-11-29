-- |
module PrettyPrint where

import Prelude
import qualified Data.Text as T
import Data.Function ((&))

-- |
sShow :: Show a => a -> String
sShow a = pformat 0 $! show a -- T.unpack $! pShow a

-- |
tShow :: Show a => a -> T.Text
tShow a = T.pack $ sShow a

-- |
pformat :: Int -> String -> String
pformat ident s =
  let
    nextIsDedent =
      case s of
        ')':_ -> 1
        ']':_ -> 1
        '}':_ -> 1
        _ -> 0
    ind = repeat ' ' & take ((ident-nextIsDedent)*2)
    indl = repeat ' ' & take (((ident-nextIsDedent)-1)*2)
  in
  case s of
    '\n':rest -> "\n" ++ ind ++ pformat ident rest
    ':':rest -> ":\n" ++ ind ++ pformat ident rest
    ';':rest -> ";\n" ++ ind ++ pformat ident rest
    ',':rest -> "\n" ++ indl ++ "," ++ pformat ident rest
    '(':')':rest -> "()" ++ pformat ident rest
    '[':']':rest -> "[]" ++ pformat ident rest
    '(':rest -> "\n" ++ ind ++ "(" ++ pformat (ident+1) rest
    '[':rest -> "\n" ++ ind ++ "[" ++ pformat (ident+1) rest
    '{':rest -> "\n" ++ ind ++ "{" ++ pformat (ident+1) rest
    ')':rest -> ")\n" ++ indl ++ pformat (ident-1) rest
    ']':rest -> "]\n" ++ indl ++ pformat (ident-1) rest
    '}':rest -> "}\n" ++ indl ++ pformat (ident-1) rest
    x:rest -> x : pformat ident rest
    [] -> ""


