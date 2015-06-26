{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

------------------------------------------------------------------------------
-- |
-- Module: QuickBooks
--
-- For more information, see:
--
--   * QuickBooks API Reference:
--     <https://developer.intuit.com/docs/api/accounting>.
--
------------------------------------------------------------------------------

module QuickBooks
  ( -- * Authentication and authorization
    OAuthToken(..)
  , getAccessTokens
  , getTempTokens
  , authorizationURLForToken
  , cancelOAuthAuthorization
    -- * Transaction entities
    -- ** Invoices
    -- *** Types
  , Invoice(..)
  , defaultInvoice
  , InvoiceId
  , Reference(..)
  , reference
  , CustomerRef
  , ItemRef
  , Line
  , SalesItemLineDetail(..)
  , salesItemLineDetail
  , salesItemLine
    -- *** CRUD an invoice
  , createInvoice
  , readInvoice
  , updateInvoice
  , deleteInvoice
    -- *** Send an invoice via email
  , EmailAddress
  , emailAddress
  , sendInvoice
  , OAuthVerifier(..)
  , QuickBooksResponse(..)  
  ) where

import QuickBooks.Authentication
import QuickBooks.Types        ( APIConfig(..)
                               , CallbackURL
                               , Invoice(..)
                               , InvoiceId(..)
                               , QuickBooksRequest(..)
                               , QuickBooksResponse(..)
                               , SyncToken
                               , OAuthToken(..)
                               , QuickBooksQuery
                               , OAuthVerifier(..)
                               , DeletedInvoice
                               , ItemRef
                               , SalesItemLineDetail(..)
                               , salesItemLineDetail
                               , Reference(..)
                               , reference
                               , defaultInvoice
                               , CustomerRef
                               , Line
                               , salesItemLine)
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
-- >>> import Data
--
-- >>> :set -XOverloadedStrings
-- >>> maybeTestOAuthToken <- lookupTestOAuthTokenFromEnv
-- >>> let oAuthToken = maybe (error "") id maybeTestOAuthToken

-- | Create an invoice.
--
-- Example:
--
-- >>> import Data.Maybe (fromJust)
-- >>> :{
-- do resp <- createInvoice oAuthToken testInvoice
--    case resp of
--      Left err -> putStrLn $ "My custom error message: " ++ err
--      Right (QuickBooksInvoiceResponse invoice) -> do
--        deleteInvoice oAuthToken (fromJust (invoiceId invoice)) (fromJust (invoiceSyncToken invoice))
--        putStrLn "I created an invoice!"
-- :}
-- I created an invoice!
--
-- Note that we deleted the item we created using 'deleteInvoice'.

createInvoice :: OAuthToken -> Invoice -> IO (Either String (QuickBooksResponse Invoice))
createInvoice tok = queryQuickBooks tok . CreateInvoice

-- | Retrieve the details of an invoice that has been previously created.
--
-- Example:
--
-- First, we create an invoice (see 'createInvoice'):
--
-- >>> import Data.Maybe (fromJust)
-- >>> Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
--
-- Then, we read the invoice and test that it is the same invoice we created:
--
-- >>> let cInvoiceId = fromJust (invoiceId cInvoice)
-- >>> :{
-- do eitherReadInvoice <- readInvoice oAuthToken cInvoiceId
--    case eitherReadInvoice of
--      Left _ -> return False
--      Right (QuickBooksInvoiceResponse rInvoice) -> return (cInvoice == rInvoice)
-- :}
-- True
--
-- Finally, we delete the invoice we created:
--
-- >>> deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))

readInvoice ::  OAuthToken -> InvoiceId -> IO (Either String (QuickBooksResponse Invoice))
readInvoice tok = queryQuickBooks tok . ReadInvoice

-- | Update an invoice.
--
-- Example:
--
-- First, we create an invoice (see 'createInvoice'):
--
-- >>> import Data.Maybe (fromJust)
-- >>> Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
--
-- Then, we update the customer reference of the invoice:
--
-- >>> let nInvoice = cInvoice { invoiceCustomerRef = Reference Nothing Nothing "1" }
-- >>> :{
-- do eitherUpdateInvoice <- updateInvoice oAuthToken nInvoice
--    case eitherUpdateInvoice of
--      Left _ -> return False
--      Right (QuickBooksInvoiceResponse uInvoice) ->
--        return (invoiceCustomerRef cInvoice == invoiceCustomerRef uInvoice)
-- :}
-- False
--
-- Finally, we delete the invoice we created:
--
-- >>> deleteInvoice oAuthToken (fromJust (invoiceId cInvoice)) (fromJust (invoiceSyncToken cInvoice))

updateInvoice ::  OAuthToken -> Invoice -> IO (Either String (QuickBooksResponse Invoice))
updateInvoice tok = queryQuickBooks tok . UpdateInvoice

-- | Delete an invoice.
--
-- Example:
--
-- First, we create an invoice (see 'createInvoice'):
--
-- >>> import Data.Maybe (fromJust)
-- >>> Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
--
-- Then, we delete it:
--
-- >>> let cInvoiceId = fromJust (invoiceId cInvoice)
-- >>> let cInvoiceSyncToken = fromJust (invoiceSyncToken cInvoice)
-- >>> :{
-- do eitherDeleteInvoice <- deleteInvoice oAuthToken cInvoiceId cInvoiceSyncToken
--    case eitherDeleteInvoice of
--      Left e -> putStrLn e
--      Right _ -> putStrLn "I deleted an invoice!"
-- :}
-- I deleted an invoice!

deleteInvoice ::  OAuthToken -> InvoiceId -> SyncToken -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoice tok iId = queryQuickBooks tok . DeleteInvoice iId

-- | Send an invoice via email.
--
-- Example:
--
-- First, we create an invoice (see 'createInvoice'):
--
-- >>> import Data.Maybe (fromJust)
-- >>> Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
--
-- Then, we send the invoice via email:
--
-- >>> let cInvoiceId = fromJust (invoiceId cInvoice)
-- >>> let testEmail = fromJust (emailAddress "test@test.com")
-- >>> :{
-- do eitherSendInvoice <- sendInvoice oAuthToken cInvoiceId testEmail
--    case eitherSendInvoice of
--      Left e -> putStrLn e
--      Right _ -> putStrLn "I sent an invoice!"
-- :}
-- I sent an invoice!
--
-- Finally, we delete the invoice we created:
--
-- >>> deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))

sendInvoice ::  OAuthToken -> InvoiceId -> EmailAddress -> IO (Either String (QuickBooksResponse Invoice))
sendInvoice tok invId = queryQuickBooks tok . SendInvoice invId

-- | Get temporary tokens to request permission.
--
-- Example:
--
-- >>> :{
-- do eitherTempTokens <- getTempTokens "localhost"
--    case eitherTempTokens of
--      Left e -> putStrLn e
--      Right _ -> putStrLn "I got my request tokens!"
-- :}
-- I got my request tokens!

getTempTokens :: CallbackURL -> IO (Either String (QuickBooksResponse OAuthToken))
getTempTokens = queryQuickBooks (OAuthToken "" "") . GetTempOAuthCredentials

-- | Exchange oauth_verifier for access tokens
getAccessTokens :: OAuthToken -> OAuthVerifier -> IO (Either String (QuickBooksResponse OAuthToken))
getAccessTokens tempToken = queryQuickBooks tempToken . GetAccessTokens

-- | Invalidate an OAuth access token and disconnect from QuickBooks.
cancelOAuthAuthorization :: OAuthToken -> IO (Either String (QuickBooksResponse ()))
cancelOAuthAuthorization tok = queryQuickBooks tok DisconnectQuickBooks

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
