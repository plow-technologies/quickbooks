{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Customer
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

module QuickBooks.Customer
  ( queryCustomerRequest
  )
  where

import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types

import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept)
import Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)


-- GET /v3/company/<companyID>/query=<selectStatement>

queryCustomerRequest :: APIEnv 
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomerRequest tok queryCustomerName = do
  let apiConfig = ?apiConfig
  let uriComponent =  escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let queryURI = parseUrl $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherAllCustomers = Aeson.eitherDecode (responseBody resp)
  case eitherAllCustomers of
    Left er -> return (Left er)
    Right (QuickBooksCustomerResponse allCustomers) ->
      return $ Right $ QuickBooksCustomerResponse $
        filter (\Customer{..} -> customerDisplayName == queryCustomerName) allCustomers
  where
    query :: String
    query = "SELECT * FROM Customer"

queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]
