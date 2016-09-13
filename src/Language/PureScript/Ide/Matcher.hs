-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Matcher
-- Description : Matchers for psc-ide commands
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Matchers for psc-ide commands
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}

module Language.PureScript.Ide.Matcher
       ( Matcher
       , runMatcher
       -- for tests
       , flexMatcher
       ) where

import           Protolude

import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           Text.EditDistance
import           Text.Regex.TDFA               ((=~))


type ScoredMatch a = (Match a, Double)

newtype Matcher a = Matcher (Endo [Match a]) deriving (Monoid)

instance FromJSON (Matcher IdeDeclarationAnn) where
  parseJSON = withObject "matcher" $ \o -> do
    (matcher :: Maybe Text) <- o .:? "matcher"
    case matcher of
      Just "flex" -> do
        params <- o .: "params"
        flexMatcher <$> params .: "search"
      Just "distance" -> do
        params <- o .: "params"
        distanceMatcher
          <$> params .: "search"
          <*> params .: "maximumDistance"
      Just _ -> mzero
      Nothing -> return mempty

-- | Matches any occurence of the search string with intersections
--
-- The scoring measures how far the matches span the string where
-- closer is better.
-- Examples:
--   flMa matches flexMatcher. Score: 14.28
--   sons matches sortCompletions. Score: 6.25
flexMatcher :: Text -> Matcher IdeDeclarationAnn
flexMatcher p = mkMatcher (flexMatch p)

distanceMatcher :: Text -> Int -> Matcher IdeDeclarationAnn
distanceMatcher q maxDist = mkMatcher (distanceMatcher' q maxDist)

distanceMatcher' :: Text -> Int -> [Match IdeDeclarationAnn] -> [ScoredMatch IdeDeclarationAnn]
distanceMatcher' q maxDist = mapMaybe go
  where
    go m = let d = dist (T.unpack y)
               y = identifierFromIdeDeclaration (discardAnn (unwrapMatch m))
          in if d <= maxDist
             then Just (m, 1 / fromIntegral d)
             else Nothing
    dist = levenshteinDistance defaultEditCosts (T.unpack q)

mkMatcher :: ([Match a] -> [ScoredMatch a]) -> Matcher a
mkMatcher matcher = Matcher . Endo  $ fmap fst . sortCompletions . matcher

runMatcher :: Matcher a -> [Match a] -> [Match a]
runMatcher (Matcher m)= appEndo m

sortCompletions :: [ScoredMatch a] -> [ScoredMatch a]
sortCompletions = sortBy (flip compare `on` snd)

flexMatch :: Text -> [Match IdeDeclarationAnn] -> [ScoredMatch IdeDeclarationAnn]
flexMatch = mapMaybe . flexRate

flexRate :: Text -> Match IdeDeclarationAnn -> Maybe (ScoredMatch IdeDeclarationAnn)
flexRate p c = do
  score <- flexScore p (identifierFromIdeDeclaration (discardAnn (unwrapMatch c)))
  return (c, score)

-- FlexMatching ala Sublime.
-- Borrowed from: http://cdewaka.com/2013/06/fuzzy-pattern-matching-in-haskell/
--
-- By string =~ pattern we'll get the start of the match and the length of
-- the matchas a (start, length) tuple if there's a match.
-- If match fails then it would be (-1,0)
flexScore :: Text -> Text -> Maybe Double
flexScore pat str =
  case T.uncons pat of
    Nothing -> Nothing
    Just (first', p) ->
      case TE.encodeUtf8 str =~ TE.encodeUtf8 pat' :: (Int, Int) of
        (-1,0) -> Nothing
        (start,len) -> Just $ calcScore start (start + len)
      where
        escapedPattern :: [Text]
        escapedPattern = map escape (T.unpack p)

        -- escape prepends a backslash to "regexy" characters to prevent the
        -- matcher from crashing when trying to build the regex
        escape :: Char -> Text
        escape c = if c `elem` T.unpack "[\\^$.|?*+(){}"
                   then T.pack ['\\', c]
                   else T.singleton c
        -- This just interleaves the search pattern with .*
        -- abcd[*] -> a.*b.*c.*d.*[*]
        pat' = escape first' <> foldMap (<> ".*") escapedPattern
        calcScore start end =
          100.0 / fromIntegral ((1 + start) * (end - start + 1))
