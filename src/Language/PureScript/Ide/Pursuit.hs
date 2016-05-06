-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Pursuit
-- Description : Pursuit client for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Pursuit client for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings   #-}

module Language.PureScript.Ide.Pursuit where

import           Prelude                       ()
import           Prelude.Compat

import qualified Control.Exception             as E
import           Data.Aeson
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (fromStrict)
import           Data.Foldable                 (toList)
import           Data.Maybe                    (mapMaybe)
import           Data.Monoid                   ((<>))
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Language.PureScript.Ide.Types
import           Network.HTTP.Types.Header     (hAccept)
import           Pipes.HTTP
import qualified Pipes.Prelude                 as P

-- We need to remove trailing dots because Pursuit will return a 400 otherwise
-- TODO: remove this when the issue is fixed at Pursuit
queryPursuit :: Text -> IO ByteString
queryPursuit q = do
  let qClean = T.dropWhileEnd (== '.') q
  req' <- parseUrl "http://pursuit.purescript.org/search"
  let req = req'
        { queryString=("q=" <> (fromString . T.unpack) qClean)
        , requestHeaders=[(hAccept, "application/json")]
        }
  m <- newManager tlsManagerSettings
  withHTTP req m $ \resp ->
    P.fold (<>) "" id $ responseBody resp


handler :: HttpException -> IO [a]
handler StatusCodeException{} = pure []
handler _ = pure []

searchPursuitForDeclarations :: Text -> IO [PursuitResponse]
searchPursuitForDeclarations query =
    (do r <- queryPursuit query
        let results' = decode (fromStrict r) :: Maybe Array
        case results' of
          Nothing -> pure []
          Just results -> pure (mapMaybe isDeclarationResponse (map fromJSON (toList results)))) `E.catch`
    handler
  where
    isDeclarationResponse (Success a@DeclarationResponse{}) = Just a
    isDeclarationResponse _ = Nothing

findPackagesForModuleIdent :: Text -> IO [PursuitResponse]
findPackagesForModuleIdent query =
  (do r <- queryPursuit query
      let results' = decode (fromStrict r) :: Maybe Array
      case results' of
        Nothing -> pure []
        Just results -> pure (mapMaybe isModuleResponse (map fromJSON (toList results)))) `E.catch`
  handler
  where
    isModuleResponse (Success a@ModuleResponse{}) = Just a
    isModuleResponse _ = Nothing
