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

import QuickBooks.Types          (APIConfig(..)
                                 ,Invoice
                                 ,InvoiceId(..)
                                 ,QuickBooksResponse
                                 ,SyncToken(..)
                                 ,DeletedInvoice(..))
import QuickBooks.Authentication
import Data.Aeson                (encode, eitherDecode, object, Value(String))
import Data.String.Interpolate   (i)
import Network.HTTP.Client       (Manager
                                 ,httpLbs
                                 ,parseUrl
                                 ,Request(..)
                                 ,RequestBody(..)
                                 ,Response(responseBody)
                                 ,getUri)
import Network.HTTP.Types.Header (hAccept,hContentType)
import Data.String (fromString)
import System.Log.FastLogger (LoggerSet, LogStr, pushLogStr, flushLogStr)
import Data.Thyme
import System.Locale 
import Data.Monoid ((<>))


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
  req' <- oauthSignRequest req{ method = "POST"
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

logAPICall :: (?logger :: LoggerSet) => Request -> IO ()
logAPICall req = do 
  now <- getCurrentTime
  let formattedTime = fromString $ formatTime defaultTimeLocale rfc822DateFormat now
  pushLogStr ?logger (requestLogLine req formattedTime)
  flushLogStr ?logger

invoiceURITemplate :: APIConfig -> String
invoiceURITemplate APIConfig{..} = [i|https://#{hostname}/v3/company/#{companyId}/invoice/|]

-- Log line format - TimeStamp [Request Method] [Request URL] [Request Body]
requestLogLine :: Request -> LogStr -> LogStr
requestLogLine req formattedTime =
  let body = case requestBody req of
              (RequestBodyLBS bs) -> show bs 
              (RequestBodyBS bs)  -> show bs
              _                   -> ""
  in formattedTime <> fromString [i|  [#{method req}] [#{getUri req}] [#{body}]\n|]
