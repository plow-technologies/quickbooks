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
  , createCustomerRequest
  , readCustomerRequest
  , updateCustomerRequest
  , deleteCustomerRequest
  )
  where

import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types
import QuickBooks.QBText

import           Data.Aeson                (encode, eitherDecode)
import           Data.String.Interpolate   (i)
import           Data.Text                 (Text)
import           Network.HTTP.Client
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)



-- | Create a new customer
createCustomerRequest :: APIEnv
                     => OAuthToken
                     -> Customer
                     -> IO (Either String (QuickBooksResponse [Customer]))
createCustomerRequest tok = postCustomer tok


-- | Read a customer by id
readCustomerRequest ::  APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
readCustomerRequest tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{customerURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp


-- | Update a customer.  (Supply a new customer with the same id as the old customer to replace the fields in it)
updateCustomerRequest :: APIEnv
                     => OAuthToken
                     -> Customer
                     -> IO (Either String (QuickBooksResponse [Customer]))
updateCustomerRequest tok = postCustomer tok


-- | Delete a customer
deleteCustomerRequest :: APIEnv
                     => OAuthToken
                     -> Customer
                     -> IO (Either String (QuickBooksResponse [Customer]))
deleteCustomerRequest tok cCustomer = do
  let nCustomer = cCustomer { customerActive = (Just False)}
  postCustomer tok nCustomer


-- Post handles create/update/'delete' in the api
postCustomer :: APIEnv
            => OAuthToken
            -> Customer
            -> IO (Either String (QuickBooksResponse [Customer]))
postCustomer tok customer = do
  let apiConfig = ?apiConfig
  req <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{customerURITemplate apiConfig}?minorversion=4|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode customer
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp


-- GET /v3/company/<companyID>/query=<selectStatement>

queryCustomerRequest :: APIEnv 
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomerRequest tok queryCustomerName = do
  let apiConfig = ?apiConfig
  let uriComponent =  escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherAllCustomers = eitherDecode (responseBody resp)
  case eitherAllCustomers of
    Left er -> return (Left er)
    Right (QuickBooksCustomerResponse allCustomers) ->
      return $ Right $ QuickBooksCustomerResponse $
        filter (\Customer{..} -> (textFromQBText customerDisplayName) == queryCustomerName) allCustomers
  where
    query :: String
    query = "SELECT * FROM Customer"

queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]


-- Template for Customer API Calls
--   / for read
--   ? update create 'delete'
customerURITemplate :: APIConfig -> String
customerURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/customer|]
