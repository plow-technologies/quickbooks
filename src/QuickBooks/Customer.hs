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
  , queryMaxCustomerRequest
  , countCustomerRequest
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
postCustomer (OAuth2 tok) customer = postCustomerOAuth2 tok customer

--- OAuth 1 ---
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

--- OAuth 2 ---
postCustomerOAuth2 :: APIEnv
                  => OAuth2.AccessToken
                  -> Customer
                  -> IO (Either String (QuickBooksResponse [Customer]))
postCustomerOAuth2 tok customer = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{customerURITemplate apiConfig}|]
  -- Made for providing an error log
  req' <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{customerURITemplate apiConfig}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI customer
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp


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


queryMaxCustomerRequest :: APIEnv
                 => OAuthTokens
                 -> Int
                 -> IO (Either String (QuickBooksResponse [Customer]))
queryMaxCustomerRequest (OAuth1 tok) startIndex = queryMaxCustomerRequestOAuth tok startIndex
queryMaxCustomerRequest (OAuth2 tok) startIndex = queryMaxCustomerRequestOAuth2 tok startIndex

--- OAuth 1 ---
queryMaxCustomerRequestOAuth :: APIEnv
                              => OAuthToken
                              -> Int
                              -> IO (Either String (QuickBooksResponse [Customer]))
queryMaxCustomerRequestOAuth tok startIndex = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query} #{pagination}|]
  let queryURI = parseUrl $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundCustomers = eitherDecode (responseBody resp)
  case eitherFoundCustomers of
    Left er -> return (Left er)
    Right (QuickBooksCustomerResponse foundCustomers) ->
      return $ Right $ QuickBooksCustomerResponse $ foundCustomers
        -- filter (\Customer{..} -> itemName == queryCustomerName) FoundCustomers
  where
    query :: String
    query = "SELECT * FROM Customer"
    pagination :: String
    pagination = [i| startposition #{startIndex} maxresults 1000|]

--- OAuth 2 ---
queryMaxCustomerRequestOAuth2 :: APIEnv
                              => OAuth2.AccessToken
                              -> Int
                              -> IO (Either String (QuickBooksResponse [Customer]))
queryMaxCustomerRequestOAuth2 tok startIndex = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query} #{pagination}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  -- Made for logging
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
          let eitherFoundCustomers = eitherDecode resp
          case eitherFoundCustomers of
            Left er -> return (Left er)
            Right (QuickBooksCustomerResponse foundCustomers) ->
              return $ Right $ QuickBooksCustomerResponse $ foundCustomers
                -- filter (\Customer{..} -> itemName == queryCustomerName) FoundCustomers
  where
    query :: String
    query = "SELECT * FROM Customer"
    pagination :: String
    pagination = [i| startposition #{startIndex} maxresults 1000|]


countCustomerRequest :: APIEnv
                 => OAuthTokens
                 -> IO (Either String (QuickBooksResponse Int))
countCustomerRequest (OAuth1 tok) = countCustomerRequestOAuth tok
countCustomerRequest (OAuth2 tok) = countCustomerRequestOAuth2 tok

--- OAuth 1 ---
countCustomerRequestOAuth :: APIEnv
                 => OAuthToken
                 -> IO (Either String (QuickBooksResponse Int))
countCustomerRequestOAuth tok = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let queryURI = parseUrl $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundCount = eitherDecode (responseBody resp)
  case eitherFoundCount of
    Left er -> return (Left er)
    Right (QuickBooksCountResponse foundCount) ->
      return $ Right $ QuickBooksCountResponse $ foundCount
        -- filter (\Customer{..} -> itemName == queryCustomerName) FoundCustomers
  where
    query :: String
    query = "SELECT COUNT(*) FROM Customer"

countCustomerRequestOAuth2 :: APIEnv
                 => OAuth2.AccessToken
                 -> IO (Either String (QuickBooksResponse Int))
countCustomerRequestOAuth2 tok = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  -- Made for logging
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
          let eitherFoundCount = eitherDecode resp
          case eitherFoundCount of
            Left er -> return (Left er)
            Right (QuickBooksCountResponse foundCount) ->
              return $ Right $ QuickBooksCountResponse $ foundCount
                -- filter (\Customer{..} -> itemName == queryCustomerName) FoundCustomers
  where
    query :: String
    query = "SELECT COUNT(*) FROM Customer"


queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]


-- Template for Customer API Calls
--   / for read
--   ? update create 'delete'
customerURITemplate :: APIConfig -> String
customerURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/customer|]
