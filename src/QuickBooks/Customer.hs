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

import qualified Network.OAuth.OAuth2      as OAuth2
import           Data.Aeson                (encode, eitherDecode)
import           Data.ByteString.Char8
import           Data.ByteString.Lazy      (fromStrict)
import           Data.String.Interpolate   (i)
import           Data.Text                 (Text)
import           Network.HTTP.Client
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)
import           URI.ByteString



-- | Create a new customer
createCustomerRequest :: APIEnv
                     => OAuthTokens
                     -> Customer
                     -> IO (Either String (QuickBooksResponse [Customer]))
createCustomerRequest tok = postCustomer tok


-- | Read a customer by id
readCustomerRequest ::  APIEnv
                     => OAuthTokens
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
readCustomerRequest (OAuth1 tok) iId = readCustomerRequestOAuth tok iId
readCustomerRequest (OAuth2 tok) iId = readCustomerRequestOAuth2 tok iId

--- OAuth 1 ---
readCustomerRequestOAuth :: APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
readCustomerRequestOAuth tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{customerURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
readCustomerRequestOAuth2 :: APIEnv
                     => OAuth2.AccessToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
readCustomerRequestOAuth2 tok iId = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{customerURITemplate apiConfig}/#{iId}|]
  req'  <- parseUrlThrow (escapeURIString isUnescapedInURI [i|#{customerURITemplate apiConfig}/#{iId}|])
  -- Made for logging
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


-- | Update a customer.  (Supply a new customer with the same id as the old customer to replace the fields in it)
updateCustomerRequest :: APIEnv
                     => OAuthTokens
                     -> Customer
                     -> IO (Either String (QuickBooksResponse [Customer]))
updateCustomerRequest tok = postCustomer tok


-- | Delete a customer
deleteCustomerRequest :: APIEnv
                     => OAuthTokens
                     -> Customer
                     -> IO (Either String (QuickBooksResponse [Customer]))
deleteCustomerRequest tok cCustomer = do
  let nCustomer = cCustomer { customerActive = (Just False)}
  postCustomer tok nCustomer


-- Post handles create/update/'delete' in the api
postCustomer :: APIEnv
            => OAuthTokens
            -> Customer
            -> IO (Either String (QuickBooksResponse [Customer]))
postCustomer (OAuth1 tok) customer = postCustomerOAuth tok customer
postCustomer (OAuth2 tok) customer = return $ Left "Not implemented"

postCustomerOAuth :: APIEnv
                  => OAuthToken
                  -> Customer
                  -> IO (Either String (QuickBooksResponse [Customer]))
postCustomerOAuth tok customer = do
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
                     => OAuthTokens
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomerRequest (OAuth1 tok) name = queryCustomerRequestOAuth tok name
queryCustomerRequest (OAuth2 tok) name = queryCustomerRequestOAuth2 tok name

queryCustomerRequestOAuth :: APIEnv 
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomerRequestOAuth tok queryCustomerName = do
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
        Prelude.filter (\Customer{..} -> (textFromQBText customerDisplayName) == queryCustomerName) allCustomers
  where
    query :: String
    query = "SELECT * FROM Customer"


queryCustomerRequestOAuth2 :: APIEnv 
                            => OAuth2.AccessToken
                            -> Text
                            -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomerRequestOAuth2 tok queryCustomerName = do
  let apiConfig = ?apiConfig
  let uriComponent =  escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  -- Made for logging
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          let eitherAllCustomers = eitherDecode resp
          case eitherAllCustomers of
            Left er -> return (Left er)
            Right (QuickBooksCustomerResponse allCustomers) ->
              return $ Right $ QuickBooksCustomerResponse $
                Prelude.filter (\Customer{..} -> (textFromQBText customerDisplayName) == queryCustomerName) allCustomers
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
