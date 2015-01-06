{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks
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

module QuickBooks
  ( createInvoice
  , readInvoice
  , updateInvoice
  , deleteInvoice
  , sendInvoice
  , getAccessTokens
  , getTempTokens
  , EmailAddress
  , emailAddress
  ) where

import QuickBooks.Authentication
import QuickBooks.Types        ( APIConfig(..)
                               , CallbackURL
                               , Invoice
                               , InvoiceId
                               , QuickBooksRequest(..)
                               , QuickBooksResponse(..)
                               , SyncToken
                               , OAuthToken
                               , QuickBooksQuery
                               , OAuthVerifier
                               , DeletedInvoice)
import Control.Applicative     ((<$>),(<*>), (<|>))
import Control.Arrow           (second)
import Data.ByteString.Char8   (pack)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client     (newManager)
import System.Environment      (getEnvironment)
import Text.Email.Validate     (EmailAddress, emailAddress)


import QuickBooks.Invoice      ( createInvoiceRequest
                               , deleteInvoiceRequest
                               , readInvoiceRequest
                               , updateInvoiceRequest
                               , sendInvoiceRequest
                               )
import QuickBooks.Logging      (apiLogger, getLogger)

   
-- | Create an invoice.
createInvoice :: Invoice -> IO (Either String (QuickBooksResponse Invoice))
createInvoice = queryQuickBooks . CreateInvoice

-- | Read an invoice.
readInvoice :: InvoiceId -> IO (Either String (QuickBooksResponse Invoice))
readInvoice = queryQuickBooks . ReadInvoice

-- | Update an invoice.
updateInvoice :: Invoice -> IO (Either String (QuickBooksResponse Invoice))
updateInvoice = queryQuickBooks . UpdateInvoice

-- | Delete an invoice.
deleteInvoice :: InvoiceId -> SyncToken -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoice iId = queryQuickBooks . DeleteInvoice iId

sendInvoice :: InvoiceId -> EmailAddress -> IO (Either String (QuickBooksResponse Invoice))
sendInvoice invId = queryQuickBooks . SendInvoice invId

-- | Get temporary tokens to request permission
getTempTokens :: CallbackURL -> IO (Either String (QuickBooksResponse OAuthToken))
getTempTokens = queryQuickBooks . GetTempOAuthCredentials

-- | Exchange oauth_verifier for access tokens
getAccessTokens :: OAuthVerifier -> OAuthToken -> IO (Either String (QuickBooksResponse OAuthToken))
getAccessTokens oauthVerifier tempToken  = queryQuickBooks $ GetAccessTokens oauthVerifier tempToken

queryQuickBooks :: QuickBooksQuery a -> IO (Either String (QuickBooksResponse a))
queryQuickBooks query = do
  apiConfig <- readAPIConfig
  manager   <- newManager tlsManagerSettings
  logger    <-  getLogger apiLogger
  let ?apiConfig = apiConfig
  let ?manager   = manager
  let ?logger    = logger
  case query of
    (CreateInvoice invoice)                   -> createInvoiceRequest invoice
    (UpdateInvoice invoice)                   -> updateInvoiceRequest invoice
    (ReadInvoice invoiceId)                   -> readInvoiceRequest invoiceId
    (DeleteInvoice invoiceId syncToken)       -> deleteInvoiceRequest invoiceId syncToken
    (SendInvoice invoiceId emailAddr)         -> sendInvoiceRequest invoiceId emailAddr
    (GetTempOAuthCredentials callbackURL)     -> getTempOAuthCredentialsRequest callbackURL
    (GetAccessTokens oauthVerifier tempToken) -> getAccessTokensRequest oauthVerifier tempToken
   
readAPIConfig :: IO APIConfig
readAPIConfig = do
  env <- getEnvironment
  case lookupAPIConfig env of
    Just config -> return config
    Nothing     -> fail "INTUIT_COMPANY_ID,INTUIT_CONSUMER_KEY,INTUIT_CONSUMER_SECRET,INTUIT_TOKEN,INTUIT_SECRET,INTUIT_HOSTNAME must be set"

lookupAPIConfig :: [(String, String)] -> Maybe APIConfig
lookupAPIConfig environment = APIConfig <$> lookup "INTUIT_COMPANY_ID" env
                                        <*> lookup "INTUIT_CONSUMER_KEY" env
                                        <*> lookup "INTUIT_CONSUMER_SECRET" env
                                        <*> lookup "INTUIT_TOKEN" env
                                        <*> lookup "INTUIT_SECRET" env
                                        <*> lookup "INTUIT_HOSTNAME" env
                                        <*> (lookup "INTUIT_API_LOGGING_ENABLED" env <|> Just "true")
    where env = map (second pack) environment
