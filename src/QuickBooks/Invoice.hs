{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE PolyKinds         #-}
------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Requests
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

module QuickBooks.Invoice
 ( createInvoiceRequest
 , readInvoiceRequest
 , updateInvoiceRequest
 , deleteInvoiceRequest
 , sendInvoiceRequest
 ) where

import qualified Network.OAuth.OAuth2      as OAuth2
import qualified Text.Email.Validate       as Email (EmailAddress, toByteString)

import           Data.ByteString.Char8
import           Data.Aeson                (encode, eitherDecode, object, Value(String))
import           Data.String.Interpolate   (i)
import           Network.HTTP.Client       (httpLbs
                                           ,parseUrlThrow
                                           ,Request(..)
                                           ,RequestBody(..)
                                           ,Response(responseBody))
import           Network.HTTP.Types.Header (hAccept,hContentType)
import           Network.URI               ( escapeURIString
                                           , isUnescapedInURI)
import           URI.ByteString

import           QuickBooks.Authentication
import           QuickBooks.Types

import QuickBooks.Logging  (logAPICall)

-- | Create an invoice.
createInvoiceRequest :: APIEnv
                     => OAuthTokens
                     -> Invoice
                     -> IO (Either String (QuickBooksResponse Invoice))
createInvoiceRequest tok = postInvoice tok

-- | Update an invoice.
updateInvoiceRequest :: APIEnv
                     => OAuthTokens
                     -> Invoice
                     -> IO (Either String (QuickBooksResponse Invoice))
updateInvoiceRequest tok = postInvoice tok

-- | Read an invoice.
readInvoiceRequest :: APIEnv
                   => OAuthTokens
                   -> InvoiceId
                   -> IO (Either String (QuickBooksResponse Invoice))
readInvoiceRequest (OAuth1 tok) iId = readInvoiceRequestOAuth tok iId
readInvoiceRequest (OAuth2 tok) iId = readInvoiceRequestOAuth2 tok iId

--- OAuth 1 ---
readInvoiceRequestOAuth :: APIEnv
                   => OAuthToken
                   -> InvoiceId
                   -> IO (Either String (QuickBooksResponse Invoice))
readInvoiceRequestOAuth tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
readInvoiceRequestOAuth2 :: APIEnv
                   => OAuth2.AccessToken
                   -> InvoiceId
                   -> IO (Either String (QuickBooksResponse Invoice))
readInvoiceRequestOAuth2 tok iId = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}|]
  -- Made for logging
  req' <- parseUrlThrow (escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}|])
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


-- | Delete an invoice.
deleteInvoiceRequest :: APIEnv
                     => OAuthTokens
                     -> InvoiceId
                     -> SyncToken
                     -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoiceRequest (OAuth1 tok) iId syncToken = deleteInvoiceRequestOAuth tok iId syncToken
deleteInvoiceRequest (OAuth2 tok) iId syncToken = deleteInvoiceRequestOAuth2 tok iId syncToken


--- OAuth 1 ---
deleteInvoiceRequestOAuth :: APIEnv
                     => OAuthToken
                     -> InvoiceId
                     -> SyncToken
                     -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoiceRequestOAuth tok iId syncToken = do
  let apiConfig = ?apiConfig
  req  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}?operation=delete|]
  req' <- oauthSignRequest tok req{ method = "POST"
                                  , requestBody    = RequestBodyLBS $ encode body
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp
  where
    body = object [ ("Id", String (unInvoiceId iId))
                  , ("SyncToken", String (unSyncToken syncToken))
                  ]

--- OAuth 2 ---
deleteInvoiceRequestOAuth2 :: APIEnv
                     => OAuth2.AccessToken
                     -> InvoiceId
                     -> SyncToken
                     -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoiceRequestOAuth2 tok iId syncToken = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{invoiceURITemplate apiConfig}?operation=delete|]
  -- Made for logging
  req'  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}?operation=delete|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI body
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp
  where
    body = object [ ("Id", String (unInvoiceId iId))
                  , ("SyncToken", String (unSyncToken syncToken))
                  ]

-- | email and invoice
sendInvoiceRequest :: APIEnv
                   => OAuthTokens
                   -> InvoiceId
                   -> Email.EmailAddress
                   -> IO (Either String (QuickBooksResponse Invoice))
sendInvoiceRequest (OAuth1 tok) iId emailAddr = sendInvoiceRequestOAuth tok iId emailAddr
sendInvoiceRequest (OAuth2 tok) iId emailAddr = return $ Left "Not implemented " -- sendInvoiceRequestOAuth2 tok iId emailAddr

--- OAuth 1 ---
sendInvoiceRequestOAuth :: APIEnv
                   => OAuthToken
                   -> InvoiceId
                   -> Email.EmailAddress
                   -> IO (Either String (QuickBooksResponse Invoice))
sendInvoiceRequestOAuth tok iId emailAddr =  do
  let apiConfig = ?apiConfig
  req  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}/send?sendTo=#{Email.toByteString emailAddr}|]
  req' <- oauthSignRequest tok req{ method = "POST"
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     ]
                                  }
  logAPICall req'
  resp <-  httpLbs req' ?manager
  return $ eitherDecode $ responseBody resp

invoiceURITemplate :: APIConfig -> String
invoiceURITemplate APIConfig{..} = [i|https://#{hostname}/v3/company/#{companyId}/invoice/|]

--- OAuth 2 ---
-- sendInvoiceRequestOAuth2 :: APIEnv
--                    => OAuth2.AccessToken
--                    -> InvoiceId
--                    -> Email.EmailAddress
--                    -> IO (Either String (QuickBooksResponse Invoice))
-- sendInvoiceRequestOAuth2 tok iId emailAddr =  do
--   let apiConfig = ?apiConfig
--   let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}/send?sendTo=#{Email.toByteString emailAddr}|]
--   -- Made for logging
--   req'  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}/send?sendTo=#{Email.toByteString emailAddr}|]
--   case eitherQueryURI of
--     Left err -> return (Left . show $ err)
--     Right queryURI -> do
--       -- Make the call
--       eitherResponse <- qbAuthPostBS ?manager tok queryURI ------ !!! Something goes here !!! -------
--       logAPICall req'
--       case eitherResponse of
--         (Left err) -> return (Left . show $ err)
--         (Right resp) -> do
--           return $ eitherDecode resp

-- invoiceURITemplate :: APIConfig -> String
-- invoiceURITemplate APIConfig{..} = [i|https://#{hostname}/v3/company/#{companyId}/invoice/|]


----- Post Invoice -----
postInvoice :: APIEnv
            => OAuthTokens
            -> Invoice
            -> IO (Either String (QuickBooksResponse Invoice))
postInvoice (OAuth1 tok) invoice = postInvoiceOAuth tok invoice
postInvoice (OAuth2 tok) invoice = postInvoiceOAuth2 tok invoice

--- OAuth 1 ---
postInvoiceOAuth :: APIEnv
            => OAuthToken
            -> Invoice
            -> IO (Either String (QuickBooksResponse Invoice))
postInvoiceOAuth tok invoice = do
  let apiConfig = ?apiConfig
  req <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode invoice
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
postInvoiceOAuth2 :: APIEnv
            => OAuth2.AccessToken
            -> Invoice
            -> IO (Either String (QuickBooksResponse Invoice))
postInvoiceOAuth2 tok invoice = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{invoiceURITemplate apiConfig}|]
  -- Made for logging
  req' <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{invoiceURITemplate apiConfig}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI invoice
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp