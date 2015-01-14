{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

------------------------------------------------------------------------------
-- |
-- Module: QuickBooks
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
  , authorizationURLForToken
  , cancelOAuthAuthorization
  , OAuthToken
  , SalesItemLineDetail(..)
  , Invoice(..)
  , InvoiceId
  ) where

import QuickBooks.Authentication
import QuickBooks.Types        ( APIConfig(..)
                               , CallbackURL
                               , Invoice(..)
                               , InvoiceId
                               , QuickBooksRequest(..)
                               , QuickBooksResponse(..)
                               , SyncToken
                               , OAuthToken(..)
                               , QuickBooksQuery
                               , OAuthVerifier
                               , DeletedInvoice
                               , SalesItemLineDetail(..))
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

-- $setup
--
-- >>> import QuickBooksSpec
-- >>> import Data
--
-- >>> :set -XOverloadedStrings
-- >>> maybeTestOAuthToken <- lookupTestOAuthTokenFromEnv
-- >>> let oAuthToken = maybe (error "") id maybeTestOAuthToken

-- | 
-- >>> :{
-- do  
--   resp <- createInvoice oAuthToken testInvoice
--   case resp of
--     Right (QuickBooksInvoiceResponse invoice) -> putStrLn "I created an invoice!"
--     Left err -> putStrLn $ "My custom error message: " ++ err
-- :}
-- I created an invoice!

-- | Create an invoice.
createInvoice :: OAuthToken -> Invoice -> IO (Either String (QuickBooksResponse Invoice))
createInvoice tok = (queryQuickBooks tok) . CreateInvoice

-- | Read an invoice.
readInvoice ::  OAuthToken -> InvoiceId -> IO (Either String (QuickBooksResponse Invoice))
readInvoice tok = (queryQuickBooks tok) . ReadInvoice

-- | Update an invoice.
updateInvoice ::  OAuthToken -> Invoice -> IO (Either String (QuickBooksResponse Invoice))
updateInvoice tok = (queryQuickBooks tok) . UpdateInvoice

-- | Delete an invoice.
deleteInvoice ::  OAuthToken -> InvoiceId -> SyncToken -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoice tok iId = (queryQuickBooks tok) . DeleteInvoice iId

-- | Send an invoice
sendInvoice ::  OAuthToken -> InvoiceId -> EmailAddress -> IO (Either String (QuickBooksResponse Invoice))
sendInvoice tok invId = (queryQuickBooks tok) . SendInvoice invId

-- | Get temporary tokens to request permission
getTempTokens :: CallbackURL -> IO (Either String (QuickBooksResponse OAuthToken))
getTempTokens = (queryQuickBooks (OAuthToken "" "")) . GetTempOAuthCredentials

-- | Exchange oauth_verifier for access tokens
getAccessTokens :: OAuthToken -> OAuthVerifier -> IO (Either String (QuickBooksResponse OAuthToken))
getAccessTokens tempToken oauthVerifier  = (queryQuickBooks tempToken) $ GetAccessTokens oauthVerifier

cancelOAuthAuthorization :: OAuthToken -> IO (Either String (QuickBooksResponse ()))
cancelOAuthAuthorization tok = (queryQuickBooks tok) $ DisconnectQuickBooks

queryQuickBooks :: OAuthToken -> QuickBooksQuery a -> IO (Either String (QuickBooksResponse a))
queryQuickBooks tok query = do
  apiConfig <- readAPIConfig
  manager   <- newManager tlsManagerSettings
  logger    <-  getLogger apiLogger
  let ?apiConfig = apiConfig
  let ?manager   = manager
  let ?logger    = logger
  case query of
    (CreateInvoice invoice)                   -> createInvoiceRequest tok invoice
    (UpdateInvoice invoice)                   -> updateInvoiceRequest tok invoice
    (ReadInvoice invoiceId)                   -> readInvoiceRequest tok invoiceId
    (DeleteInvoice invoiceId syncToken)       -> deleteInvoiceRequest tok invoiceId syncToken
    (SendInvoice invoiceId emailAddr)         -> sendInvoiceRequest tok invoiceId emailAddr
    (GetTempOAuthCredentials callbackURL)     -> getTempOAuthCredentialsRequest callbackURL
    (GetAccessTokens oauthVerifier)           -> getAccessTokensRequest tok oauthVerifier
    (DisconnectQuickBooks)                    -> disconnectRequest tok
   
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
