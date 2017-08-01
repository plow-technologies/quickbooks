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

import Data.Aeson                (eitherDecode)
import Data.String.Interpolate   (i)
import Data.Text                 (Text)
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept)
import Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)

-- | Read a bundle by id
readBundleRequest ::  APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Bundle]))
readBundleRequest tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{bundleURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp


-- GET /v3/company/<companyID>/query=<selectStatement>

-- Searches by name
queryBundleRequest :: APIEnv
                 => OAuthToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Bundle]))
queryBundleRequest tok queryBundleName = do
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
