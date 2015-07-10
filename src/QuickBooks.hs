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
    getAccessTokens
  , getAccessTokens'
  , getTempTokens
  , getTempTokens'
  , authorizationURLForToken
  , cancelOAuthAuthorization
  , cancelOAuthAuthorization'
    -- * Transaction entities
    -- ** Invoices
    -- *** Types
  , module QuickBooks.Types
    -- *** CRUD an invoice
  , createInvoice
  , createInvoice'
  , readInvoice
  , readInvoice'
  , updateInvoice
  , updateInvoice'
  , deleteInvoice
  , deleteInvoice'
    -- *** Send an invoice via email
  , EmailAddress
  , emailAddress
  , sendInvoice
  , sendInvoice'
    -- *** Read in configuration files
  , readAPIConfigFromFile
  , readAppConfigFromFile
    -- * Name list entities
    -- ** Customer
  , queryCustomer
  , queryCustomer'
    -- ** Line
  , queryItem
  , queryItem'
  ) where

import QuickBooks.Authentication
import QuickBooks.Types hiding (EmailAddress,emailAddress)

import Control.Applicative     ((<$>),(<*>), (<|>))
import Control.Arrow           (second)
import Data.ByteString.Char8   (pack)
import Data.Maybe              (fromJust)
import Data.Text               (Text)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client     (newManager)
import System.Environment      (getEnvironment)
import Text.Email.Validate     (EmailAddress, emailAddress)


import QuickBooks.Customer
import QuickBooks.Invoice      ( createInvoiceRequest
                               , deleteInvoiceRequest
                               , readInvoiceRequest
                               , updateInvoiceRequest
                               , sendInvoiceRequest
                               )
import QuickBooks.Item
import QuickBooks.Logging      (apiLogger, getLogger)
import Data.Yaml (ParseException, decodeFileEither)

-- $setup
--
-- >>> import Data
--
-- >>> :set -XOverloadedStrings
-- >>> maybeTestOAuthToken <- lookupTestOAuthTokenFromEnv
-- >>> let oAuthToken = maybe (error "") id maybeTestOAuthToken

-- |
--
-- >>> :{
--   do eitherQueryCustomer <-
--        queryCustomer oAuthToken "Rondonuwu Fruit and Vegi"
--      case eitherQueryCustomer of
--        Right (QuickBooksCustomerResponse (customer:_)) ->
--          print (customerId customer)
--        _ ->
--          putStrLn "Nothing"
-- :}
-- Just "21"

queryCustomer
  :: OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomer tok =
  queryQuickBooks tok . QueryCustomer

queryCustomer'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomer' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryCustomer

-- |
--
-- >>> :{
--   do eitherQueryItem <- queryItem oAuthToken "Hours"
--      case eitherQueryItem of
--        Right (QuickBooksItemResponse (item:_)) ->
--          print (itemId item)
--        _ ->
--          putStrLn "Nothing"
-- :}
-- Just "2"

queryItem
  :: OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Item]))
queryItem tok =
  queryQuickBooks tok . QueryItem

queryItem'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Item]))
queryItem' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryItem

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

-- | Like createInvoice but accepts an APIConfig rather than reading it from the environment
createInvoice' :: APIConfig -> AppConfig -> OAuthToken -> Invoice -> IO (Either String (QuickBooksResponse Invoice))
createInvoice' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . CreateInvoice


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

-- | Like readInvoice but accepts an APIConfig rather than reading it from the environment
readInvoice' :: APIConfig -> AppConfig -> OAuthToken -> InvoiceId -> IO (Either String (QuickBooksResponse Invoice))
readInvoice' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . ReadInvoice

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

-- | Like updateInvoice but accepts an APIConfig rather than reading it from the environment
updateInvoice' :: APIConfig -> AppConfig -> OAuthToken -> Invoice -> IO (Either String (QuickBooksResponse Invoice))
updateInvoice' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . UpdateInvoice

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

-- | Like deleteInvoice but accepts an APIConfig rather than reading it from the environment
deleteInvoice' :: APIConfig -> AppConfig -> OAuthToken -> InvoiceId -> SyncToken -> IO (Either String (QuickBooksResponse DeletedInvoice))
deleteInvoice' apiConfig appConfig tok iId = queryQuickBooks' apiConfig appConfig tok . DeleteInvoice iId


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
--      Left e  -> putStrLn e
--      Right _ -> putStrLn "I sent an invoice!"
-- :}
-- I sent an invoice!
--
-- Finally, we delete the invoice we created:
--
-- >>> deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))

sendInvoice ::  OAuthToken -> InvoiceId -> EmailAddress -> IO (Either String (QuickBooksResponse Invoice))
sendInvoice tok invId = queryQuickBooks tok . SendInvoice invId

sendInvoice' :: APIConfig
             -> AppConfig
             -> OAuthToken
             -> InvoiceId
             -> EmailAddress
             -> IO (Either String (QuickBooksResponse Invoice))
sendInvoice' apiConfig appConfig tok invId  =
  queryQuickBooks' apiConfig appConfig tok . SendInvoice invId

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
-- ...
-- I got my request tokens!

getTempTokens :: CallbackURL -> IO (Either String (QuickBooksResponse OAuthToken))
getTempTokens =
  queryQuickBooksOAuth Nothing . GetTempOAuthCredentials

getTempTokens' :: AppConfig -> CallbackURL -> IO (Either String (QuickBooksResponse OAuthToken))
getTempTokens' appConfig =
  queryQuickBooksOAuth' appConfig Nothing . GetTempOAuthCredentials
 
-- | Exchange oauth_verifier for access tokens
getAccessTokens :: OAuthToken -> OAuthVerifier -> IO (Either String (QuickBooksResponse OAuthToken))
getAccessTokens tempToken =
  queryQuickBooksOAuth (Just tempToken) . GetAccessTokens

getAccessTokens' :: AppConfig          -- Your application's consumer key and consumer secret
                 -> OAuthToken         -- The temporary OAuth tokens obtained from getTempTokens
                 -> OAuthVerifier      -- The OAuthVerifier returned by QuickBooks when it calls your callback
                 -> IO (Either String (QuickBooksResponse OAuthToken))
getAccessTokens' appConfig tempToken = do
  queryQuickBooksOAuth' appConfig (Just tempToken) . GetAccessTokens

-- | Invalidate an OAuth access token and disconnect from QuickBooks.
cancelOAuthAuthorization :: OAuthToken -> IO (Either String (QuickBooksResponse ()))
cancelOAuthAuthorization tok =
  queryQuickBooksOAuth (Just tok)  DisconnectQuickBooks

cancelOAuthAuthorization' :: AppConfig
                          -> OAuthToken
                          -> IO (Either String (QuickBooksResponse ()))
cancelOAuthAuthorization' appConfig tok =
  queryQuickBooksOAuth' appConfig (Just tok) DisconnectQuickBooks

queryQuickBooks :: OAuthToken -> QuickBooksQuery a -> IO (Either String (QuickBooksResponse a))
queryQuickBooks tok query = do
  apiConfig <- readAPIConfig
  appConfig <- readAppConfig
  queryQuickBooks' apiConfig appConfig tok query

queryQuickBooks' :: APIConfig -> AppConfig -> OAuthToken -> QuickBooksQuery a -> IO (Either String (QuickBooksResponse a))
queryQuickBooks' apiConfig appConfig tok query = do
  manager   <- newManager tlsManagerSettings
  logger    <- getLogger apiLogger
  let ?appConfig = appConfig
  let ?apiConfig = apiConfig
  let ?manager   = manager
  let ?logger    = logger
  case query of
    CreateInvoice invoice               -> createInvoiceRequest tok invoice
    ReadInvoice _invoiceId              -> readInvoiceRequest tok _invoiceId
    UpdateInvoice invoice               -> updateInvoiceRequest tok invoice
    DeleteInvoice _invoiceId syncToken  -> deleteInvoiceRequest tok _invoiceId syncToken
    SendInvoice _invoiceId emailAddr    -> sendInvoiceRequest tok _invoiceId emailAddr    
    QueryCustomer queryCustomerName     -> queryCustomerRequest tok queryCustomerName
    QueryItem queryItemName             -> queryItemRequest tok queryItemName

queryQuickBooksOAuth :: Maybe OAuthToken
                     -> QuickBooksOAuthQuery a
                     -> IO (Either String (QuickBooksResponse a))
queryQuickBooksOAuth maybeOAuthToken query = do
  appConfig <- readAppConfig
  queryQuickBooksOAuth' appConfig maybeOAuthToken query 

queryQuickBooksOAuth' :: AppConfig
                      -> Maybe OAuthToken
                      -> QuickBooksOAuthQuery a
                      -> IO (Either String (QuickBooksResponse a))
queryQuickBooksOAuth' appConfig maybeOauthToken query = do
  manager   <- newManager tlsManagerSettings
  logger    <- getLogger apiLogger
  let ?appConfig = appConfig
  let ?manager   = manager
  let ?logger    = logger
  case query of
    (GetTempOAuthCredentials callbackURL) -> getTempOAuthCredentialsRequest callbackURL
    (GetAccessTokens oauthVerifier)       -> getAccessTokensRequest (fromJust maybeOauthToken) oauthVerifier
    DisconnectQuickBooks                  -> disconnectRequest (fromJust maybeOauthToken)

readAPIConfig :: IO APIConfig
readAPIConfig = do
  env <- getEnvironment
  case lookupAPIConfig env of
    Just config -> return config
    Nothing     -> fail "The environment variables INTUIT_COMPANY_ID,INTUIT_TOKEN,INTUIT_SECRET, and INTUIT_HOSTNAME must be set"

readAppConfig :: IO AppConfig
readAppConfig = do
  env <- getEnvironment
  case lookupAppConfig env of
    Just config -> return config
    Nothing     -> fail "The evironment variables INTUIT_CONSUMER_KEY and INTUIT_CONSUMER_SECRET must be set"

lookupAPIConfig :: [(String, String)] -> Maybe APIConfig
lookupAPIConfig environment = APIConfig <$> lookup "INTUIT_COMPANY_ID" env
                                        <*> lookup "INTUIT_TOKEN" env
                                        <*> lookup "INTUIT_SECRET" env
                                        <*> lookup "INTUIT_HOSTNAME" env
                                        <*> (lookup "INTUIT_API_LOGGING_ENABLED" env <|> Just "true")
    where env = map (second pack) environment

readAPIConfigFromFile :: FilePath -> IO (Either ParseException APIConfig)
readAPIConfigFromFile = decodeFileEither

readAppConfigFromFile :: FilePath -> IO (Either ParseException AppConfig)
readAppConfigFromFile = decodeFileEither

lookupAppConfig :: [(String, String)] -> Maybe AppConfig
lookupAppConfig environment = AppConfig <$> lookup "INTUIT_CONSUMER_KEY" env
                                        <*> lookup "INTUIT_CONSUMER_SECRET" env
    where env = map (second pack) environment
