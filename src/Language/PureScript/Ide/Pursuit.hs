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

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.Pursuit
  ( searchPursuitForDeclarations
  , findPackagesForModuleIdent
  ) where

import           Protolude

import qualified Control.Exception             as E
import           Data.Aeson                    (Array, Result (Success), decode,
                                                fromJSON)
import           Language.PureScript.Ide.Types
import           Network.HTTP.Types.Header     (hAccept)
import           Pipes.HTTP                    (HttpException, newManager,
                                                parseRequest, queryString,
                                                requestHeaders, responseBody,
                                                tlsManagerSettings, withHTTP)
import qualified Pipes.Prelude                 as P

queryPursuit :: Text -> IO ByteString
queryPursuit q = do
    req' <- parseRequest "http://pursuit.purescript.org/search"
    let req = req'
          { queryString= "q=" <> toS q
          , requestHeaders=[(hAccept, "application/json")]
          }
    m <- newManager tlsManagerSettings
    withHTTP req m $ \resp ->
      P.fold (<>) "" identity (responseBody resp)

handler :: HttpException -> IO [a]
handler _ = pure []

searchPursuitForDeclarations :: Text -> IO [PursuitResponse]
searchPursuitForDeclarations query = E.handle handler $ do
    r <- queryPursuit query
    let results' = decode (toS r) :: Maybe Array
    case results' of
        Nothing -> pure []
        Just results ->
          pure (mapMaybe (isDeclarationResponse . fromJSON) (toList results))
  where
    isDeclarationResponse (Success a@DeclarationResponse{}) = Just a
    isDeclarationResponse _ = Nothing

findPackagesForModuleIdent :: Text -> IO [PursuitResponse]
findPackagesForModuleIdent query = E.handle handler $ do
    r <- queryPursuit query
    let results' = decode (toS r) :: Maybe Array
    case results' of
        Nothing -> pure []
        Just results ->
          pure (mapMaybe (isModuleResponse . fromJSON) (toList results))
  where
    isModuleResponse (Success a@ModuleResponse{}) = Just a
    isModuleResponse _ = Nothing
