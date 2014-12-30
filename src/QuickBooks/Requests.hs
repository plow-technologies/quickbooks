{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module QuickBooks.Requests
 ( createInvoiceRequest
 , readInvoiceRequest
 , updateInvoiceRequest
 , deleteInvoiceRequest
 ) where

import           QuickBooks.Types
import           Data.Aeson
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import           Web.Authenticate.OAuth hiding (delete)
import           Data.String.Interpolate

createInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        ) => Invoice
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

updateInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        ) => Invoice
                          -> IO (Either String (QuickBooksResponse Invoice))
updateInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}|]
  let req' = req{method = "POST", requestBody = RequestBodyLBS $ encode invoice}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

readInvoiceRequest :: ( ?apiConfig :: APIConfig
                      , ?manager :: Manager
                      ) => InvoiceId
                        -> IO (Either String (QuickBooksResponse Invoice))
readInvoiceRequest iId = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i|#{invoiceURITemplate apiConfig}#{unInvoiceId iId}|]
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

deleteInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
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
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp
  where
   body = object [ ("Id", String (unInvoiceId iId))
                 , ("SyncToken", String (unSyncToken syncToken))
                 ]

oauthSignRequest :: (?apiConfig :: APIConfig) => Request -> IO Request
oauthSignRequest = signOAuth oauthApp credentials
    where
    credentials = newCredential (oauthToken ?apiConfig)
                                (oauthSecret ?apiConfig)
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                           , oauthConsumerSecret = consumerSecret ?apiConfig }

invoiceURITemplate :: APIConfig -> String
invoiceURITemplate apiConfig = [i|https://#{hostname apiConfig}/v3/company/#{companyId apiConfig}/invoice/|]
