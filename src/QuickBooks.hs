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
  , createCustomer
  , createCustomer'
  , readCustomer
  , readCustomer'
  , updateCustomer
  , updateCustomer'
  , deleteCustomer
  , deleteCustomer'
  , queryCustomer
  , queryCustomer'
  , queryCustomerCount
  , queryCustomerCount'
  , queryMaxCustomersFrom
  , queryMaxCustomersFrom'
    -- ** Line
  , createItem
  , createItem'
  , readItem
  , readItem'
  , updateItem
  , updateItem'
  , deleteItem
  , deleteItem'
  , queryItem
  , queryItem'
  , queryItemCount
  , queryItemCount'
  , queryMaxItemsFrom
  , queryMaxItemsFrom'
  , readBundle
  , readBundle'
  , queryBundle
  , queryBundle'
  , createCategory
  , createCategory'
  , readCategory
  , readCategory'
  , updateCategory
  , updateCategory'
  , deleteCategory
  , deleteCategory'
  , queryCategory
  , queryCategory'
  , queryCategoryCount
  , queryCategoryCount'
  , queryMaxCategoriesFrom
  , queryMaxCategoriesFrom'
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
import QuickBooks.Bundle
import QuickBooks.Category
import QuickBooks.Logging      (apiLogger, getLogger)
import Data.Yaml (ParseException, decodeFileEither)


------------------
---- Customer ----
------------------

-- Create Customer --
createCustomer :: OAuthToken -> Customer -> IO (Either String (QuickBooksResponse [Customer]))
createCustomer tok = queryQuickBooks tok . CreateCustomer

-- | Like createCustomer but accepts an APIConfig rather than reading it from the environment
createCustomer' :: APIConfig -> AppConfig -> OAuthToken -> Customer -> IO (Either String (QuickBooksResponse [Customer]))
createCustomer' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . CreateCustomer

-- Read Customer --
readCustomer :: OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Customer]))
readCustomer tok = queryQuickBooks tok . ReadCustomer

-- | Like readCustomer but accepts an APIConfig rather than reading it from the environment
readCustomer' :: APIConfig -> AppConfig -> OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Customer]))
readCustomer' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . ReadCustomer

-- Update Customer --
updateCustomer :: OAuthToken -> Customer -> IO (Either String (QuickBooksResponse [Customer]))
updateCustomer tok = queryQuickBooks tok . UpdateCustomer

-- | Like updateCustomer but accepts an APIConfig rather than reading it from the environment
updateCustomer' :: APIConfig -> AppConfig -> OAuthToken -> Customer -> IO (Either String (QuickBooksResponse [Customer]))
updateCustomer' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . UpdateCustomer

-- Delete Customer --
deleteCustomer ::  OAuthToken -> Customer -> IO (Either String (QuickBooksResponse [Customer]))
deleteCustomer tok = queryQuickBooks tok . DeleteCustomer

-- | Like deleteCustomer but accepts an APIConfig rather than reading it from the environment
deleteCustomer' :: APIConfig -> AppConfig -> OAuthToken -> Customer -> IO (Either String (QuickBooksResponse [Customer]))
deleteCustomer' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . DeleteCustomer


-- Unsued DocTest (removed 6-26-17)
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

queryCustomerCount
  :: OAuthToken
  -> IO (Either String (QuickBooksResponse Int))
queryCustomerCount tok =
  queryQuickBooks tok QueryCountCustomer

queryCustomerCount'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> IO (Either String (QuickBooksResponse Int))
queryCustomerCount' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok QueryCountCustomer


queryMaxCustomersFrom
  :: OAuthToken
  -> Int
  -> IO (Either String (QuickBooksResponse [Customer]))
queryMaxCustomersFrom tok =
  queryQuickBooks tok . QueryMaxCustomersFrom

queryMaxCustomersFrom'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Int
  -> IO (Either String (QuickBooksResponse [Customer]))
queryMaxCustomersFrom' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryMaxCustomersFrom

--------------
---- Item ----
--------------

-- Create Item --
createItem :: OAuthToken -> Item -> IO (Either String (QuickBooksResponse [Item]))
createItem tok = queryQuickBooks tok . CreateItem

-- | Like createItem but accepts an APIConfig rather than reading it from the environment
createItem' :: APIConfig -> AppConfig -> OAuthToken -> Item -> IO (Either String (QuickBooksResponse [Item]))
createItem' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . CreateItem

-- Read Item --
readItem :: OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Item]))
readItem tok = queryQuickBooks tok . ReadItem

-- | Like readItem but accepts an APIConfig rather than reading it from the environment
readItem' :: APIConfig -> AppConfig -> OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Item]))
readItem' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . ReadItem

-- Update Item --
updateItem :: OAuthToken -> Item -> IO (Either String (QuickBooksResponse [Item]))
updateItem tok = queryQuickBooks tok . UpdateItem

-- | Like updateItem but accepts an APIConfig rather than reading it from the environment
updateItem' :: APIConfig -> AppConfig -> OAuthToken -> Item -> IO (Either String (QuickBooksResponse [Item]))
updateItem' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . UpdateItem

-- Delete Item --
deleteItem ::  OAuthToken -> Item -> IO (Either String (QuickBooksResponse [Item]))
deleteItem tok = queryQuickBooks tok . DeleteItem

-- | Like deleteItem but accepts an APIConfig rather than reading it from the environment
deleteItem' :: APIConfig -> AppConfig -> OAuthToken -> Item -> IO (Either String (QuickBooksResponse [Item]))
deleteItem' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . DeleteItem

-- Unused DocTest (removed 6-26-17)
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


queryItemCount
  :: OAuthToken
  -> IO (Either String (QuickBooksResponse Int))
queryItemCount tok =
  queryQuickBooks tok QueryCountItem

queryItemCount'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> IO (Either String (QuickBooksResponse Int))
queryItemCount' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok QueryCountItem


queryMaxItemsFrom
  :: OAuthToken
  -> Int
  -> IO (Either String (QuickBooksResponse [Item]))
queryMaxItemsFrom tok =
  queryQuickBooks tok . QueryMaxItemsFrom

queryMaxItemsFrom'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Int
  -> IO (Either String (QuickBooksResponse [Item]))
queryMaxItemsFrom' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryMaxItemsFrom

-- queryAllItems :: OAuthToken -> IO (Either String ([QuickBooksItemResponse [Item]]))
-- queryAllItems otk = queryQuickBooks tok . QueryAllItems

----------------
-- Bundle R/Q --
----------------

-- Read Bundle --
readBundle :: OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Bundle]))
readBundle tok = queryQuickBooks tok . ReadBundle

-- | Like readBundle but accepts an APIConfig rather than reading it from the environment
readBundle' :: APIConfig -> AppConfig -> OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Bundle]))
readBundle' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . ReadBundle

-- Query Bundle
queryBundle
  :: OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Bundle]))
queryBundle tok =
  queryQuickBooks tok . QueryBundle

queryBundle'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Bundle]))
queryBundle' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryBundle


--------------------
-- Category CRUDQ --
--------------------

-- Create Category --
createCategory :: OAuthToken -> Category -> IO (Either String (QuickBooksResponse [Category]))
createCategory tok = queryQuickBooks tok . CreateCategory

-- | Like createCategory but accepts an APIConfig rather than reading it from the environment
createCategory' :: APIConfig -> AppConfig -> OAuthToken -> Category -> IO (Either String (QuickBooksResponse [Category]))
createCategory' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . CreateCategory

-- Read Category --
readCategory :: OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Category]))
readCategory tok = queryQuickBooks tok . ReadCategory

-- | Like readCategory but accepts an APIConfig rather than reading it from the environment
readCategory' :: APIConfig -> AppConfig -> OAuthToken -> Text -> IO (Either String (QuickBooksResponse [Category]))
readCategory' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . ReadCategory

-- Update Category --
updateCategory :: OAuthToken -> Category -> IO (Either String (QuickBooksResponse [Category]))
updateCategory tok = queryQuickBooks tok . UpdateCategory

-- | Like updateCategory but accepts an APIConfig rather than reading it from the environment
updateCategory' :: APIConfig -> AppConfig -> OAuthToken -> Category -> IO (Either String (QuickBooksResponse [Category]))
updateCategory' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . UpdateCategory

-- Delete Category --
deleteCategory ::  OAuthToken -> Category -> IO (Either String (QuickBooksResponse DeletedCategory))
deleteCategory tok = queryQuickBooks tok . DeleteCategory

-- | Like deleteCategory but accepts an APIConfig rather than reading it from the environment
deleteCategory' :: APIConfig -> AppConfig -> OAuthToken -> Category -> IO (Either String (QuickBooksResponse DeletedCategory))
deleteCategory' apiConfig appConfig tok = queryQuickBooks' apiConfig appConfig tok . DeleteCategory

queryCategory
  :: OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Category]))
queryCategory tok =
  queryQuickBooks tok . QueryCategory

queryCategory'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Category]))
queryCategory' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryCategory








queryCategoryCount
  :: OAuthToken
  -> IO (Either String (QuickBooksResponse Int))
queryCategoryCount tok =
  queryQuickBooks tok QueryCountCategory

queryCategoryCount'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> IO (Either String (QuickBooksResponse Int))
queryCategoryCount' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok QueryCountCategory

  
queryMaxCategoriesFrom
  :: OAuthToken
  -> Int
  -> IO (Either String (QuickBooksResponse [Category]))
queryMaxCategoriesFrom tok =
  queryQuickBooks tok . QueryMaxCategoriesFrom

queryMaxCategoriesFrom'
  :: APIConfig
  -> AppConfig
  -> OAuthToken
  -> Int
  -> IO (Either String (QuickBooksResponse [Category]))
queryMaxCategoriesFrom' apiConfig appConfig tok =
  queryQuickBooks' apiConfig appConfig tok . QueryMaxCategoriesFrom



-- Unused DocTest (removed 6-26-17)
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

-- Unused DocTest (removed 6-26-17)
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

-- Unused DocTest (removed 6-26-17)
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

-- Unused DocTest (removed 6-26-17)
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


-- Unused DocTest (removed 6-26-17)
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

-- Unused DocTest (removed 6-26-17)
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
    CreateCustomer customer             -> createCustomerRequest tok customer
    ReadCustomer cId                    -> readCustomerRequest tok cId
    UpdateCustomer customer             -> updateCustomerRequest tok customer
    DeleteCustomer customer             -> deleteCustomerRequest tok customer    
    QueryCustomer queryCustomerName     -> queryCustomerRequest tok queryCustomerName
    QueryCountCustomer                  -> countCustomerRequest tok
    QueryMaxCustomersFrom startIndex    -> queryMaxCustomerRequest tok startIndex
    CreateItem item                     -> createItemRequest tok item
    ReadItem iId                        -> readItemRequest tok iId
    UpdateItem item                     -> updateItemRequest tok item
    DeleteItem item                     -> deleteItemRequest tok item
    QueryItem queryItemName             -> queryItemRequest tok queryItemName
    QueryCountItem                      -> countItemRequest tok
    QueryMaxItemsFrom startIndex        -> queryMaxItemRequest tok startIndex
    --QueryAllItems                       -> queryAllItemRequest tok
    ReadBundle iId                      -> readBundleRequest tok iId
    QueryBundle queryBundleName         -> queryBundleRequest tok queryBundleName
    CreateCategory category             -> createCategoryRequest tok category
    ReadCategory iId                    -> readCategoryRequest tok iId
    UpdateCategory category             -> updateCategoryRequest tok category
    DeleteCategory category             -> deleteCategoryRequest tok category
    QueryCategory queryCategoryName     -> queryCategoryRequest tok queryCategoryName
    QueryCountCategory                  -> countCategoryRequest tok
    QueryMaxCategoriesFrom startIndex   -> queryMaxCategoryRequest tok startIndex

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
  eitherAPIConfig <- readAPIConfigFromFile $ "config/quickbooksConfig.yml"
  case eitherAPIConfig of
    Left err -> fail "The config variables INTUIT_COMPANY_ID,INTUIT_TOKEN,INTUIT_SECRET, and INTUIT_HOSTNAME must be set"
    Right config -> return config
  -- env <- getEnvironment
  -- case lookupAPIConfig env of
  --   Just config -> return config
  --   Nothing     -> fail "The environment variables INTUIT_COMPANY_ID,INTUIT_TOKEN,INTUIT_SECRET, and INTUIT_HOSTNAME must be set"

readAppConfig :: IO AppConfig
readAppConfig = do
  eitherAppConfig <- readAppConfigFromFile $ "config/quickbooksConfig.yml"
  case eitherAppConfig of
    Left err -> fail "The config variables INTUIT_CONSUMER_KEY and INTUIT_CONSUMER_SECRET must be set"
    Right config -> return config
  -- env <- getEnvironment
  -- case lookupAppConfig env of
  --   Just config -> return config
  --   Nothing     -> fail "The evironment variables INTUIT_CONSUMER_KEY and INTUIT_CONSUMER_SECRET must be set"

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
