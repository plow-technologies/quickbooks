{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

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
 ) where

import Data.Aeson                (encode, eitherDecode, object, Value(String))
import Data.String.Interpolate   (i)
import Network.HTTP.Client       (Manager
                                 ,httpLbs
                                 ,parseUrl
                                 ,Request(..)
                                 ,RequestBody(..)
                                 ,Response(responseBody))
import Network.HTTP.Types.Header (hAccept,hContentType)
import System.Log.FastLogger (LoggerSet)

import QuickBooks.Authentication
import QuickBooks.Logging (logAPICall)
import QuickBooks.Types (APIConfig(..)
                        ,Invoice
                        ,InvoiceId(..)
                        ,QuickBooksResponse
                        ,SyncToken(..)
                        ,DeletedInvoice(..))
    

-- | Create an invoice.
createInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager   :: Manager
                        , ?logger    :: LoggerSet
                        )
                     => Invoice
                     -> IO (Either String (QuickBooksResponse Invoice))
createInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req  <- parseUrl [i|#{invoiceURITemplate apiConfig}|]
  req' <- oauthSignRequest req{ method         = "POST"
                              , requestBody    = RequestBodyLBS $ encode invoice
                              , requestHeaders = [ (hAccept, "application/json")
                                                 , (hContentType, "application/json")
                                                 ]
                              }
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- | Update an invoice.
updateInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        , ?logger    :: LoggerSet
                        )
                     => Invoice
                     -> IO (Either String (QuickBooksResponse Invoice))
updateInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req <- parseUrl [i|#{invoiceURITemplate apiConfig}|]
  req' <- oauthSignRequest req{method = "POST"
                              , requestBody = RequestBodyLBS $ encode invoice
                              , requestHeaders = [ (hAccept, "application/json")
                                                 , (hContentType, "application/json")
                                                 ]
                              }
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- | Read an invoice.
readInvoiceRequest :: ( ?apiConfig :: APIConfig
                      , ?manager :: Manager
                      , ?logger    :: LoggerSet
                      )
                   => InvoiceId
                   -> IO (Either String (QuickBooksResponse Invoice))
readInvoiceRequest iId = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}|]
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- | Delete an invoice.
deleteInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager   :: Manager
                        , ?logger    :: LoggerSet
                        ) => InvoiceId
                          -> SyncToken
                          -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoiceRequest iId syncToken = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}?operation=delete|]
  req' <- oauthSignRequest req{ method = "POST"
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

invoiceURITemplate :: APIConfig -> String
invoiceURITemplate APIConfig{..} = [i|https://#{hostname}/v3/company/#{companyId}/invoice/|]
