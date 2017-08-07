{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Bundle
-- Description :
-- Copyright   :
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
--
--
------------------------------------------------------------------------------

module QuickBooks.Bundle
  ( queryBundleRequest
  , readBundleRequest
  )
  where

import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types
-- import QuickBooks.QBText

import qualified Network.OAuth.OAuth2      as OAuth2

import           Data.Aeson                (eitherDecode)
import           Data.ByteString.Char8
import           Data.String.Interpolate   (i)
import           Data.Text                 (Text)

import           Network.HTTP.Client
import           Network.HTTP.Types.Header (hAccept)
import           Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)

import           URI.ByteString

-- | Read a bundle by id
readBundleRequest ::  APIEnv
                     => OAuthTokens
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Bundle]))
readBundleRequest (OAuth1 tok) bId = readBundleRequestOAuth tok bId
readBundleRequest (OAuth2 tok) bId = readBundleRequestOAuth2 tok bId

--- OAuth 1 ---
readBundleRequestOAuth ::  APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Bundle]))
readBundleRequestOAuth tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{bundleURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
readBundleRequestOAuth2 ::  APIEnv
                     => OAuth2.AccessToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Bundle]))
readBundleRequestOAuth2 tok iId = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{bundleURITemplate apiConfig}/#{iId}|]
  -- Made for logging
  req' <- parseUrlThrow (escapeURIString isUnescapedInURI [i|#{bundleURITemplate apiConfig}/#{iId}|])
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp
-- GET /v3/company/<companyID>/query=<selectStatement>

-- Searches by name
queryBundleRequest :: APIEnv
                 => OAuthTokens
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Bundle]))
queryBundleRequest (OAuth1 tok) queryBundleName = queryBundleRequestOAuth tok queryBundleName
queryBundleRequest (OAuth2 tok) queryBundleName = queryBundleRequestOAuth2 tok queryBundleName

--- OAuth 1 ---
queryBundleRequestOAuth :: APIEnv
                 => OAuthToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Bundle]))
queryBundleRequestOAuth tok queryBundleName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{bundleSearch}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundBundles = eitherDecode (responseBody resp)
  case eitherFoundBundles of
    Left er -> return (Left er)
    Right (QuickBooksBundleResponse foundBundles) ->
      return $ Right $ QuickBooksBundleResponse $ foundBundles
        -- filter (\Bundle{..} -> bundleName == queryBundleName) FoundBundles
  where
    query :: String
    query = "SELECT * FROM Item"
    -- if Text /= "" -> Where Name='input'
    -- if Text == "" -> return all bundles
    bundleName = [i|#{queryBundleName}|]
    bundleSearch :: String
    bundleSearch = if (bundleName == "")
      then " where Type='Group'" -- All Bundles
      else [i| WHERE Type='Group' AND Name='#{queryBundleName}'|]    -- Bundle that Matchs Name

--- OAuth 2 ---
queryBundleRequestOAuth2 :: APIEnv
                 => OAuth2.AccessToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Bundle]))
queryBundleRequestOAuth2 tok queryBundleName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{bundleSearch}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          let eitherFoundBundles = eitherDecode resp
          case eitherFoundBundles of
            Left er -> return (Left er)
            Right (QuickBooksBundleResponse foundBundles) ->
              return $ Right $ QuickBooksBundleResponse $ foundBundles
                -- filter (\Bundle{..} -> bundleName == queryBundleName) FoundBundles
  where
    query :: String
    query = "SELECT * FROM Item"
    -- if Text /= "" -> Where Name='input'
    -- if Text == "" -> return all bundles
    bundleName = [i|#{queryBundleName}|]
    bundleSearch :: String
    bundleSearch = if (bundleName == "")
      then " where Type='Group'" -- All Bundles
      else [i| WHERE Type='Group' AND Name='#{queryBundleName}'|]    -- Bundle that Matchs Name

-- Template for queries
queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]

-- Template for Bundle API Calls
--   / for read
--   ? update create 'delete'
bundleURITemplate :: APIConfig -> String
bundleURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/item|]
