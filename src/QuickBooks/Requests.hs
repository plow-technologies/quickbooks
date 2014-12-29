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
import           Web.Authenticate.OAuth hiding (delete)
import           Data.String.Interpolate

createInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        ) => Invoice
                          -> IO (Either String Invoice)
createInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i| #{invoiceURITemplate apiConfig} |]
  let req' = req{method = "POST", requestBody = RequestBodyLBS $ encode invoice}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

updateInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        ) => Invoice
                          -> IO (Either String Invoice)
updateInvoiceRequest invoice = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i| #{invoiceURITemplate apiConfig} |]
  let req' = req{method = "POST", requestBody = RequestBodyLBS $ encode invoice}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

readInvoiceRequest :: ( ?apiConfig :: APIConfig
                      , ?manager :: Manager
                      ) => InvoiceId
                        -> IO (Either String Invoice)
readInvoiceRequest iId = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i| #{invoiceURITemplate apiConfig}#{unInvoiceId iId} |]
  let req' = req{method = "GET"}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp

deleteInvoiceRequest :: ( ?apiConfig :: APIConfig
                        , ?manager :: Manager
                        ) => InvoiceId
                          -> SyncToken
                          -> IO (Either String Invoice)
deleteInvoiceRequest iId syncToken = do
  let apiConfig = ?apiConfig
  req  <-  oauthSignRequest =<< parseUrl [i| #{invoiceURITemplate apiConfig}?operation=delete |]
  let req' = req{method = "POST", requestBody = RequestBodyLBS $ encode body}
  resp <-  httpLbs req' ?manager
  -- write log line ?fast-logger package? log response
  return $ eitherDecode $ responseBody resp
  where
   body = object [ ("id", String (unInvoiceId iId))
                 , ("SyncToken", Number $ fromInteger (unSyncToken syncToken))
                 ]

oauthSignRequest :: (?apiConfig :: APIConfig) => Request -> IO Request
oauthSignRequest = signOAuth oauthApp credentials
    where
    credentials = newCredential (oauthToken ?apiConfig)
                                (oauthSecret ?apiConfig)
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                           , oauthConsumerSecret = consumerSecret ?apiConfig }

invoiceURITemplate :: APIConfig -> String
invoiceURITemplate apiConfig = [i| https://#{hostname apiConfig}/v3/company/#{companyId apiConfig}/invoice|]
