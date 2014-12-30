{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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

module QuickBooks.Requests
 ( createInvoiceRequest
 , readInvoiceRequest
 , updateInvoiceRequest
 , deleteInvoiceRequest)
 where

import QuickBooks.Types          (APIConfig(..)
                                 ,Invoice
                                 ,InvoiceId(..)
                                 ,QuickBooksResponse
                                 ,SyncToken(..))
import QuickBooks.Authentication
import Data.Aeson                (encode, eitherDecode, object, Value(String))
import Data.String.Interpolate   (i)
import Network.HTTP.Client       (Manager
                                 ,httpLbs
                                 ,parseUrl
                                 ,Request(..)
                                 ,RequestBody(..)
                                 ,Response(responseBody))
import Network.HTTP.Types.Header (hAccept,hContentType)

-- | Create an invoice.
createInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        )
                     => Invoice
                     -> IO (Either String (QuickBooksResponse Invoice))
createInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req  <- parseUrl [i|#{invoiceURITemplate apiConfig}|]
  req' <- oauthSignRequest req{ method = "POST"
                              , requestBody    = RequestBodyLBS $ encode invoice
                              , requestHeaders = [ (hAccept, "application/json")
                                                 , (hContentType, "application/json")
                                                 ]
                              }
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

-- | Update an invoice.
updateInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        )
                     => Invoice
                     -> IO (Either String (QuickBooksResponse Invoice))
updateInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}|]
  let req' = req{method = "POST", requestBody = RequestBodyLBS $ encode invoice}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

-- | Read an invoice.
readInvoiceRequest :: ( ?apiConfig :: APIConfig
                      , ?manager :: Manager
                      )
                   => InvoiceId
                   -> IO (Either String (QuickBooksResponse Invoice))
readInvoiceRequest iId = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}|]
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

-- | Delete an invoice.
deleteInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        )
                     => InvoiceId
                     -> SyncToken
                     -> IO (Either String (QuickBooksResponse Invoice))
deleteInvoiceRequest iId syncToken = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}?operation=delete|]
  let req' = req{method = "POST", requestBody = RequestBodyLBS $ encode body}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp
  where
   body = object [ ("id", String (unInvoiceId iId))
                 , ("SyncToken", String (unSyncToken syncToken))
                 ]


invoiceURITemplate :: APIConfig -> String
invoiceURITemplate apiConfig = [i|https://#{hostname apiConfig}/v3/company/#{companyId apiConfig}/invoice/|]
