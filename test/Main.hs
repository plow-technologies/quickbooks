{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import QuickBooks
import QuickBooks.Logging
import QuickBooks.Types
import Test.QuickCheck.Arbitrary       (arbitrary)
import Test.QuickCheck.Gen             (generate)

import           Data.String.Interpolate   (i)
import           Data.Aeson.QQ
import           Control.Monad         (ap, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (fromJust)
import           Data.String
import qualified Data.Text             as T
import           Data.Text             (Text)
import           QuickBooks
import           QuickBooks.Authentication
import           QuickBooks.QBText
import           System.Environment    (getEnvironment)
import qualified Text.Email.Validate   as E (EmailAddress, emailAddress)
import qualified Network.OAuth.OAuth2            as OAuth2

import           Data.Aeson                (encode, eitherDecode)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)
import           URI.ByteString
import Data.Yaml (ParseException, decodeFileEither)


main :: IO ()
main = do
    eitherOath2Config <- readOAuth2Config
    case eitherOath2Config of
      Left err -> do
        putStrLn ("Error with OAuth2, defaulting to OAuth1: " ++ err)
        maybeTestOAuthToken <- lookupTestOAuthTokenFromEnv
        print maybeTestOAuthToken
        let oAuthToken' = maybe (error "") id maybeTestOAuthToken
        defaultMain $ tests (OAuth1 oAuthToken')
      Right oath2Config -> do
        putStrLn "Running with OAuth2"
        authTokens <- fetchAccessToken oath2Config
        defaultMain $ tests (OAuth2 $ OAuth2.accessToken authTokens)

tests :: OAuthTokens -> TestTree
tests tok = testGroup "API Calls" [ testCase "Post Test" $ postTest tok 
                              , testCase "Query Customer" $ queryCustomerTest tok
                              , testCase "Query Empty Customer" $ queryEmptyCustomerTest tok
                              , testCase "Query Max Customer" $ queryMaxCustomerTest tok
                              , testCase "Query Count Customer" $ queryCountCustomerTest tok
                              , testCase "Create Customer" $ createCustomerTest tok
                              , testCase "Read Customer" $ readCustomerTest tok
                              , testCase "Update Customer" $ updateCustomerTest tok
                              , testCase "Delete Customer" $ deleteCustomerTest tok
                              , testCase "Read Bundle" $ readBundleTest tok
                              , testCase "Query Bundle" $ queryBundleTest tok
                              , testCase "Query Empty Bundle" $ queryEmptyBundleTest tok
                              , testCase "Query Category" $ queryCategoryTest tok
                              , testCase "Query Empty Category" $ queryEmptyCategoryTest tok
                              , testCase "Query Count Categories" $ queryCountCategoryTest tok
                              , testCase "Query Max Categories" $ queryMaxCategoryTest tok
                              , testCase "Create Category" $ createCategoryTest tok
                              , testCase "Read Category" $ readCategoryTest tok
                              , testCase "Update Category" $ updateCategoryTest tok
                              , testCase "Delete Category" $ deleteCategoryTest tok
                              , testCase "Query Item" $ queryItemTest tok
                              , testCase "Query Count Items" $ queryCountItemTest tok
                              , testCase "Query Empty Item" $ queryEmptyItemTest tok
                              , testCase "Query Max Items" $ queryMaxItemTest tok
                              , testCase "Create Item" $ createItemTest tok
                              , testCase "Read Item" $ readItemTest tok
                              , testCase "Update Item" $ updateItemTest tok
                              , testCase "Delete Item" $ deleteItemTest tok
                              , testCase "Create Invoice" $ createInvoiceTest tok
                              , testCase "Read Invoice" $ readInvoiceTest tok
                              , testCase "Update Invoice" $ updateInvoiceTest tok
                              , testCase "Delete Invoice" $ deleteInvoiceTest tok
                              , testCase "Email Invoice" $ emailInvoiceTest tok
                              , testCase "Temp Tokens" $ tempTokenTest
                              ]

-----------  Note: There is a very small chance that they may fail due to duplicate name errors on create.
-- Tests --  Just rerun the tests and they will likely pass.
-----------

---- Post Test ----
postTest :: OAuthTokens -> Assertion
postTest tok = do
  apiConfig <- Main.readAPIConfig
  appConfig <- Main.readAppConfig
  manager   <- newManager tlsManagerSettings
  logger <- getLogger apiLogger
  let ?appConfig = appConfig
  let ?apiConfig = apiConfig
  let ?manager   = manager
  let ?logger    = logger
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|https://sandbox.api.intuit.com/quickbooks/v4/payments/charges|]
  req' <- parseUrlThrow $ (escapeURIString isUnescapedInURI [i|https://sandbox.api.intuit.com/quickbooks/v4/payments/charges|])
  case eitherQueryURI of
    Left err -> assertEither (show err) eitherQueryURI
    Right queryURI -> do
      -- Make the call
      let (OAuth2 token) = tok
      logAPICall req'
      eitherResponse <- qbAuthPostBS ?manager (token) queryURI [aesonQQ|{"amount": "10.55", "token": "bFy3h7W3D2tmOfYxl2msnLbUirY=", "currency": "USD"}|]
      case eitherResponse of
        Left err -> assertEither (show err) eitherResponse
        Right resp -> do
          assertEither "It worked?" eitherResponse
          -- (eitherDecode resp)



readAPIConfig = do
  eitherAPIConfig <- Main.readAPIConfigFromFile $ "config/quickbooksConfig.yml"
  case eitherAPIConfig of
    Left _ -> fail "The config variables companyId, hostname, and loggingEnabled must be set"
    Right config -> return config
  -- env <- getEnvironment
  -- case lookupAPIConfig env of
  --   Just config -> return config
  --   Nothing     -> fail "The environment variables INTUIT_COMPANY_ID,INTUIT_TOKEN,INTUIT_SECRET, and INTUIT_HOSTNAME must be set"

readAppConfig :: IO AppConfig
readAppConfig = do
  eitherAppConfig <- Main.readAppConfigFromFile $ "config/quickbooksConfig.yml"
  case eitherAppConfig of
    Left _ -> fail "The config variables INTUIT_CONSUMER_KEY and INTUIT_CONSUMER_SECRET must be set"
    Right config -> return config
  -- env <- getEnvironment
  -- case lookupAppConfig env of
  --   Just config -> return config
  --   Nothing     -> fail "The evironment variables INTUIT_CONSUMER_KEY and INTUIT_CONSUMER_SECRET must be set"

readAPIConfigFromFile :: FilePath -> IO (Either ParseException APIConfig)
readAPIConfigFromFile = decodeFileEither

readAppConfigFromFile :: FilePath -> IO (Either ParseException AppConfig)
readAppConfigFromFile = decodeFileEither






---- Create Customer ----
createCustomerTest :: OAuthTokens -> Assertion
createCustomerTest oAuthToken = do
  testCustomer <- makeTestCustomer
  resp <- createCustomer oAuthToken testCustomer
  case resp of
    Left err -> assertEither ("My custom error message: " ++ err) resp
    Right (QuickBooksCustomerResponse (customer:_)) -> do
      deleteCustomer oAuthToken customer
      assertEither "I created a customer!" resp

---- Read Customer ----
readCustomerTest :: OAuthTokens -> Assertion
readCustomerTest oAuthToken = do
  testCustomer <- makeTestCustomer
  Right (QuickBooksCustomerResponse (cCustomer:_)) <- createCustomer oAuthToken testCustomer
  let (Just iId) = customerId cCustomer
  eitherReadCustomer <- readCustomer oAuthToken $ textFromQBText iId
  case eitherReadCustomer of
    Left _ -> do
      deleteCustomer oAuthToken cCustomer
      assertEither "Failed to read customer" eitherReadCustomer
    Right (QuickBooksCustomerResponse (rCustomer:_)) -> do
      deleteCustomer oAuthToken cCustomer
      assertBool "Read the Customer" (customerId cCustomer == customerId rCustomer)

---- Update Customer ----
updateCustomerTest :: OAuthTokens -> Assertion
updateCustomerTest oAuthToken = do
  testCustomer <- makeTestCustomer
  Right (QuickBooksCustomerResponse (cCustomer:_)) <- createCustomer oAuthToken testCustomer
  let eitherTestQBText = filterTextForQB $ T.pack "Changed"
  case eitherTestQBText of
    Left err -> assertEither "Error making QBText in updateCustomerTest" eitherTestQBText
    Right testQBText -> do
      let nCustomer = cCustomer { customerGivenName = Just testQBText, customerId = (customerId cCustomer) }
      eitherUpdateCustomer <- updateCustomer oAuthToken nCustomer
      case eitherUpdateCustomer of
        Left _ -> do
          deleteCustomer oAuthToken cCustomer
          assertEither "Failed to update invoice" eitherUpdateCustomer
        Right (QuickBooksCustomerResponse (uCustomer:_)) -> do
          deleteCustomer oAuthToken uCustomer
          assertBool "Updated the Customer" (customerGivenName cCustomer /= customerGivenName uCustomer)

---- Delete Customer ----
deleteCustomerTest :: OAuthTokens -> Assertion
deleteCustomerTest oAuthToken = do
  -- First, we create a customer (see 'createCustomer'):
  testCustomer <- makeTestCustomer
  Right (QuickBooksCustomerResponse (cCustomer:_)) <- createCustomer oAuthToken testCustomer
  -- Then, we delete it:
  eitherDeleteCustomer <- deleteCustomer oAuthToken cCustomer
  case eitherDeleteCustomer of
    Left e -> assertEither (show e) eitherDeleteCustomer
    Right _ -> assertEither "I *deleted* a customer!" eitherDeleteCustomer


---- Query Customer ----
queryCustomerTest :: OAuthTokens -> Assertion
queryCustomerTest oAuthToken = do
  eitherQueryCustomer <- queryCustomer oAuthToken "Rondonuwu Fruit and Vegi"
  case eitherQueryCustomer of
    Left err ->
      assertEither ("Faild to query customer: " ++ err) eitherQueryCustomer
    Right (QuickBooksCustomerResponse (customer:_)) -> do
      case filterTextForQB "21" of
        Left err -> assertEither "Error making QBText in queryCustomerTest" (filterTextForQB "21")
        Right existingId ->
          assertBool (show $ customerId customer) (customerId customer == Just existingId)

---- Query Empty Customer ----
queryEmptyCustomerTest :: OAuthTokens -> Assertion
queryEmptyCustomerTest oAuthToken = do
  eitherQueryCustomer <- queryCustomer oAuthToken ""
  case eitherQueryCustomer of
    Left _ ->
      assertEither "Failed to query customer" eitherQueryCustomer
    Right (QuickBooksCustomerResponse []) -> do
      assertEither "There were no customers to query, but the test passes" eitherQueryCustomer
    Right (QuickBooksCustomerResponse (customer:_)) -> do
      assertEither "Query for a list of customers" eitherQueryCustomer

---- Query Max Customer ----
queryMaxCustomerTest :: OAuthTokens -> Assertion
queryMaxCustomerTest oAuthToken = do
  eitherQueryCustomers <- queryMaxCustomersFrom oAuthToken 1
  case eitherQueryCustomers of
    Left _ ->
      assertEither "Failed to query customer" eitherQueryCustomers
    Right (QuickBooksCustomerResponse []) -> do
      assertEither "There were no customers to query, but the test passes" eitherQueryCustomers
    Right (QuickBooksCustomerResponse (customer:_)) -> do
      assertEither "Queried for max size" eitherQueryCustomers

---- Query Count Customer ----
queryCountCustomerTest :: OAuthTokens -> Assertion
queryCountCustomerTest oAuthToken = do
  eitherCount <- queryCustomerCount oAuthToken
  case eitherCount of
    Left _ ->
      assertEither "Failed to query customer count" eitherCount
    Right (QuickBooksCountResponse size) ->
      assertEither "Queried the count" eitherCount

-----------------
-- Item CRUD-Q --
-----------------

---- Create Item ----
createItemTest :: OAuthTokens -> Assertion
createItemTest oAuthToken = do
  testItem <- makeTestItem
  resp <- createItem oAuthToken testItem
  case resp of
    Left err -> assertEither ("My custom error message: " ++ err) resp
    Right (QuickBooksItemResponse (item:_)) -> do
      deleteItem oAuthToken item
      assertEither "I created an item!" resp

---- Read Item ----
readItemTest :: OAuthTokens -> Assertion
readItemTest oAuthToken = do
  testItem <- makeTestItem
  Right (QuickBooksItemResponse (cItem:_)) <- createItem oAuthToken testItem
  let (Just iId) = itemId cItem
  eitherReadItem <- readItem oAuthToken $ textFromQBText iId
  case eitherReadItem of
    Left _ -> do
      deleteItem oAuthToken cItem
      assertEither "Failed to read item" eitherReadItem
    Right (QuickBooksItemResponse (rItem:_)) -> do
      deleteItem oAuthToken cItem
      assertBool "Read the Item" (itemId cItem == itemId rItem)

---- Update Item ----
updateItemTest :: OAuthTokens -> Assertion
updateItemTest oAuthToken = do
  testItem <- makeTestItem
  Right (QuickBooksItemResponse (cItem:_)) <- createItem oAuthToken testItem
  let nItem = cItem { itemPurchaseDesc = Just "Changed", itemId = (itemId cItem) }
  eitherUpdateItem <- updateItem oAuthToken nItem
  case eitherUpdateItem of
    Left _ -> do
      deleteItem oAuthToken cItem
      assertEither "Failed to update invoice" eitherUpdateItem
    Right (QuickBooksItemResponse (uItem:_)) -> do
      deleteItem oAuthToken uItem
      assertBool "Updated the Item" (itemPurchaseDesc cItem /= itemPurchaseDesc uItem)

---- Delete Item ----
deleteItemTest :: OAuthTokens -> Assertion
deleteItemTest oAuthToken = do
  -- First, we create an item (see 'createItem'):
  testItem <- makeTestItem
  Right (QuickBooksItemResponse (cItem:_)) <- createItem oAuthToken testItem
  -- Then, we delete it:
  eitherDeleteItem <- deleteItem oAuthToken cItem
  case eitherDeleteItem of
    Left e -> assertEither (show e) eitherDeleteItem
    Right _ -> assertEither "I *deleted* an item!" eitherDeleteItem

---- Query Item ----
queryItemTest :: OAuthTokens -> Assertion
queryItemTest oAuthToken = do
  eitherQueryItem <- queryItem oAuthToken "Hours"
  case eitherQueryItem of
    Left _ ->
      assertEither "Failed to query item" eitherQueryItem
    Right (QuickBooksItemResponse (item:_)) -> do
      case filterTextForQB "2" of
        Left err -> assertEither "Failed to create QBText in queryItemTest" (filterTextForQB "2")
        Right existingId ->
          assertBool (show $ itemId item) (itemId item == Just existingId)

---- Query Empty Item ----
queryEmptyItemTest :: OAuthTokens -> Assertion
queryEmptyItemTest oAuthToken = do
  eitherQueryItem <- queryItem oAuthToken ""
  case eitherQueryItem of
    Left _ ->
      assertEither "Failed to query item" eitherQueryItem
    Right (QuickBooksItemResponse []) -> do
      assertEither "There were no items to query, but the test passes" eitherQueryItem
    Right (QuickBooksItemResponse (item:_)) -> do
      assertEither "Query for a list of items" eitherQueryItem

---- Query Max Item ----
queryMaxItemTest :: OAuthTokens -> Assertion
queryMaxItemTest oAuthToken = do
  eitherQueryItems <- queryMaxItemsFrom oAuthToken 1
  case eitherQueryItems of
    Left _ ->
      assertEither "Failed to query item" eitherQueryItems
    Right (QuickBooksItemResponse (item:_)) -> do
      assertEither "Queried for max size" eitherQueryItems

---- Query Count Item ----
queryCountItemTest :: OAuthTokens -> Assertion
queryCountItemTest oAuthToken = do
  eitherCount <- queryItemCount oAuthToken
  case eitherCount of
    Left _ ->
      assertEither "Failed to query item count" eitherCount
    Right (QuickBooksCountResponse size) ->
      assertEither "Queried the count" eitherCount

---------------------
-- Bundle CRUD-Q --
---------------------

---- Read Bundle ----
readBundleTest :: OAuthTokens -> Assertion
readBundleTest oAuthToken = do
  case filterTextForQB "19" of
    Left err -> assertEither "Failed to create QBText in readBundleTest" (filterTextForQB "19")
    Right existingId -> do
      let existingBundleId = existingId -- this MUST be created in QB Online for the test to pass
                            -- Replace the number the id for the existing bundle
                            -- It can be obtained by querying the bundle name below
                            -- Or through the API Explorer at https://developer.intuit.com/v2/apiexplorer?apiname=V3QBO#?id=Item
                            -- With select * from item where Type='Group'
      eitherReadBundle <- readBundle oAuthToken (textFromQBText existingBundleId)
      case eitherReadBundle of
        Left _ -> do
          assertEither "Failed to read bundle" eitherReadBundle
        Right (QuickBooksBundleResponse (rBundle:_)) -> do
          let rBundleId = fromJust (bundleId rBundle)
          assertBool "Read the Bundle" ((textFromQBText rBundleId) == (textFromQBText existingBundleId))

---- Query Bundle ----
queryBundleTest :: OAuthTokens -> Assertion
queryBundleTest oAuthToken = do
  case filterTextForQB "19" of
    Left err -> assertEither "Failed to create QBText in queryBundleTest" (filterTextForQB "19")
    Right existingId -> do
      let existingBundleId = existingId -- this MUST be created in QB Online for the test to pass
                            -- Replace the number the id for the existing bundle
                            -- It can be obtained by querying the bundle name below
                            -- Or through the API Explorer at https://developer.intuit.com/v2/apiexplorer?apiname=V3QBO#?id=Item
                            -- With select * from item where Type='Group'
      eitherQueryBundle <- queryBundle oAuthToken "Bundle 1"
      case eitherQueryBundle of
        Left _ ->
          assertEither "Failed to query bundle" eitherQueryBundle
        Right (QuickBooksBundleResponse (bundle:_)) -> do
          let bundleId' = fromJust (bundleId bundle)
          assertBool (show $ bundleId') (bundleId' == existingBundleId)

---- Query Empty Bundle ----
queryEmptyBundleTest :: OAuthTokens -> Assertion
queryEmptyBundleTest oAuthToken = do
  eitherQueryBundle <- queryBundle oAuthToken ""
  case eitherQueryBundle of
    Left _ ->
      assertEither "Failed to query bundle" eitherQueryBundle
    Right (QuickBooksBundleResponse []) -> do
      assertEither "There were no bundles to query, but the test passes" eitherQueryBundle
    Right (QuickBooksBundleResponse (bundle:_)) -> do
      assertEither "Query for a list of bundles" eitherQueryBundle



---------------------
-- Category CRUD-Q --
---------------------

---- Create Category ----
createCategoryTest :: OAuthTokens -> Assertion
createCategoryTest oAuthToken = do
  testCategory <- makeTestCategory
  resp <- createCategory oAuthToken testCategory
  case resp of
    Left err -> assertEither ("My custom error message: " ++ err) resp
    Right (QuickBooksCategoryResponse (category:_)) -> do
      -- Make a sub category in the first
      case (categoryId category) of
        Nothing ->
          assertBool "Faild to get category id" False
        Just categoryId -> do
          testCategory2 <- makeTestCategory2 $ textFromQBText categoryId
          resp2 <- createCategory oAuthToken testCategory2
          case resp2 of
            Left err -> assertEither ("My custom error message: " ++ err) resp2
            Right (QuickBooksCategoryResponse (category2:_)) -> do
              -- Delete the sub category
              _ <- deleteCategory oAuthToken category2
              return ()
          -- Delete the top layer category
          deleteCategory oAuthToken category
          assertEither "I created an invoice!" resp

---- Read Category ----
readCategoryTest :: OAuthTokens -> Assertion
readCategoryTest oAuthToken = do
  testCategory <- makeTestCategory
  Right (QuickBooksCategoryResponse (cCategory:_)) <- createCategory oAuthToken testCategory
  let (Just iId) = categoryId cCategory
  eitherReadCategory <- readCategory oAuthToken $ textFromQBText iId
  case eitherReadCategory of
    Left _ -> do
      deleteCategory oAuthToken cCategory
      assertEither "Failed to read category" eitherReadCategory
    Right (QuickBooksCategoryResponse (rCategory:_)) -> do
      deleteCategory oAuthToken cCategory
      assertBool "Read the Category" (categoryId cCategory == categoryId rCategory)

---- Update Category ----
updateCategoryTest :: OAuthTokens -> Assertion
updateCategoryTest oAuthToken = do
  testCategory <- makeTestCategory
  Right (QuickBooksCategoryResponse (cCategory:_)) <- createCategory oAuthToken testCategory
  let eitherCreatedCategoryName = filterTextForQB $ T.append (textFromQBText $ categoryName cCategory) "C"
  case eitherCreatedCategoryName of
    Left err -> assertBool "Couldn't make QBText from the created QB Item?" False
    Right cCategoryName -> do
      let nCategory = cCategory { categoryName = cCategoryName, categoryId = (categoryId cCategory) }
      eitherUpdateCategory <- updateCategory oAuthToken nCategory
      case eitherUpdateCategory of
        Left _ -> do
          deleteCategory oAuthToken cCategory
          assertEither "Failed to update invoice" eitherUpdateCategory
        Right (QuickBooksCategoryResponse (uCategory:_)) -> do
          deleteCategory oAuthToken uCategory
          assertBool "Updated the Category" (categoryName cCategory /= categoryName uCategory)

---- Delete Category ----
deleteCategoryTest :: OAuthTokens -> Assertion
deleteCategoryTest oAuthToken = do
  -- First, we create an category (see 'createCategory'):
  testCategory <- makeTestCategory
  Right (QuickBooksCategoryResponse (cCategory:_)) <- createCategory oAuthToken testCategory
  -- Then, we delete it:
  eitherDeleteCategory <- deleteCategory oAuthToken cCategory
  case eitherDeleteCategory of
    Left e -> assertEither (show e) eitherDeleteCategory
    Right _ -> assertEither "I *deleted* an category!" eitherDeleteCategory

---- Query Category ----
queryCategoryTest :: OAuthTokens -> Assertion
queryCategoryTest oAuthToken = do
  eitherQueryCategory <- queryCategory oAuthToken "Cat 1"
  case eitherQueryCategory of
    Left err ->
      assertEither ("Failed to query category" ++ err) eitherQueryCategory
    Right (QuickBooksCategoryResponse []) -> do
      assertEither "The test returned no categories, but still passes" eitherQueryCategory
    Right (QuickBooksCategoryResponse (category:_)) -> do
      case filterTextForQB "20" of
        Left err -> assertEither "Faild to create QBText in queryCategoryTest" (filterTextForQB "20")
        Right existingId ->
          assertBool (show $ categoryId category) (categoryId category == (Just existingId))

---- Query Empty Category ----
queryEmptyCategoryTest :: OAuthTokens -> Assertion
queryEmptyCategoryTest oAuthToken = do
  eitherQueryCategory <- queryCategory oAuthToken ""
  case eitherQueryCategory of
    Left _ ->
      assertEither "Failed to query category" eitherQueryCategory
    Right (QuickBooksCategoryResponse []) -> do
      assertEither "There were no categories to query, but the test passes" eitherQueryCategory
    Right (QuickBooksCategoryResponse (category:_)) -> do
      assertEither "Query for a list of categories" eitherQueryCategory

---- Query Max Category ----
queryMaxCategoryTest :: OAuthTokens -> Assertion
queryMaxCategoryTest oAuthToken = do
  eitherQueryCategories <- queryMaxCategoriesFrom oAuthToken 1
  case eitherQueryCategories of
    Left _ ->
      assertEither "Failed to query category" eitherQueryCategories
    Right (QuickBooksCategoryResponse []) -> do
      assertEither "The query returned no categories, but still passes" eitherQueryCategories
    Right (QuickBooksCategoryResponse (category:_)) -> do
      assertEither "Queried for max size" eitherQueryCategories

---- Query Count Category ----
queryCountCategoryTest :: OAuthTokens -> Assertion
queryCountCategoryTest oAuthToken = do
  eitherCount <- queryCategoryCount oAuthToken
  case eitherCount of
    Left _ ->
      assertEither "Failed to query category count" eitherCount
    Right (QuickBooksCountResponse size) ->
      assertEither "Queried the count" eitherCount

---- Invoice CRUD-Email ----
createInvoiceTest :: OAuthTokens -> Assertion
createInvoiceTest oAuthToken = do
  resp <- createInvoice oAuthToken testInvoice
  case resp of
    Left err -> assertEither ("My custom error message: " ++ err) resp
    Right (QuickBooksInvoiceResponse invoice) -> do
     deleteInvoice oAuthToken (fromJust (invoiceId invoice)) (fromJust (invoiceSyncToken invoice))
     assertEither "I created an invoice!" resp

readInvoiceTest :: OAuthTokens -> Assertion
readInvoiceTest oAuthToken = do
  -- First, we create an invoice (see 'createInvoice'):
  Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
  -- Then, we read the invoice and test that it is the same invoice we created:
  let cInvoiceId = fromJust (invoiceId cInvoice)
  do
    eitherReadInvoice <- readInvoice oAuthToken cInvoiceId
    case eitherReadInvoice of
      Left _ -> do
        -- Finally, we delete the invoice we created:
        deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))
        assertEither "Failed to read created invoice" eitherReadInvoice
      Right (QuickBooksInvoiceResponse rInvoice) -> do
        -- Finally, we delete the invoice we created:
        deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))
        assertBool "Read the invoice correctly" (cInvoice == rInvoice)

updateInvoiceTest :: OAuthTokens -> Assertion
updateInvoiceTest oAuthToken = do
  --First, we create an invoice (see 'createInvoice'):
  Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
  -- Then, we update the customer reference of the invoice:
  let nInvoice = cInvoice { invoiceCustomerRef = Reference Nothing Nothing "1" }
  do
    eitherUpdateInvoice <- updateInvoice oAuthToken nInvoice
    case eitherUpdateInvoice of
      Left _ -> do
        --Finally, we delete the invoice we created:
        deleteInvoice oAuthToken (fromJust (invoiceId cInvoice)) (fromJust (invoiceSyncToken cInvoice))
        assertEither "Failed to update invoice" eitherUpdateInvoice
      Right (QuickBooksInvoiceResponse uInvoice) -> do
        --Finally, we delete the invoice we created:
        print $ invoiceCustomerRef cInvoice
        print $ invoiceCustomerRef uInvoice
        deleteInvoice oAuthToken (fromJust (invoiceId cInvoice)) (fromJust (invoiceSyncToken cInvoice))
        assertBool "Updated the invoice" (invoiceCustomerRef cInvoice /= invoiceCustomerRef uInvoice)

deleteInvoiceTest :: OAuthTokens -> Assertion
deleteInvoiceTest oAuthToken = do
  -- First, we create an invoice (see 'createInvoice'):
  Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
  -- Then, we delete it:
  let cInvoiceId = fromJust (invoiceId cInvoice)
  let cInvoiceSyncToken = fromJust (invoiceSyncToken cInvoice)
  do
    eitherDeleteInvoice <- deleteInvoice oAuthToken cInvoiceId cInvoiceSyncToken
    case eitherDeleteInvoice of
      Left e -> assertEither (show e) eitherDeleteInvoice
      Right _ -> assertEither "I deleted an invoice!" eitherDeleteInvoice

emailInvoiceTest :: OAuthTokens -> Assertion
emailInvoiceTest oAuthToken = do
  -- First, we create an invoice (see 'createInvoice'):
  Right (QuickBooksInvoiceResponse cInvoice) <- createInvoice oAuthToken testInvoice
  -- Then, we send the invoice via email:
  let cInvoiceId = fromJust (invoiceId cInvoice)
  let testEmail = fromJust (E.emailAddress "test@test.com")
  do
    eitherSendInvoice <- sendInvoice oAuthToken cInvoiceId testEmail
    case eitherSendInvoice of
      Left e  -> do
        -- Finally, we delete the invoice we created:
        deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))
        assertEither e eitherSendInvoice
      Right _ -> do
        -- Finally, we delete the invoice we created:
        deleteInvoice oAuthToken cInvoiceId (fromJust (invoiceSyncToken cInvoice))
        assertEither "I sent an invoice!" eitherSendInvoice

---- Temp Tokens ----
tempTokenTest :: Assertion
tempTokenTest = do
  -- | Get temporary tokens to request permission.
  eitherTempTokens <- getTempTokens "localhost"
  case eitherTempTokens of
    Left e -> assertEither e eitherTempTokens
    Right _ -> assertEither "I got my request tokens!" eitherTempTokens

-------------------
--HelperFunctions--
-------------------
--same as assertBool, but fails on (Left e) instead of False
assertEither :: String -> Either e a -> Assertion
assertEither msg testResult = assertBool msg (eitherToBool testResult)

eitherToBool :: Either e a -> Bool
eitherToBool (Left _) = False
eitherToBool (Right _) = True


lookupTestOAuthTokenFromEnv :: IO (Maybe OAuthToken)
lookupTestOAuthTokenFromEnv = do
  env <- getEnvironment
  return $ OAuthToken `liftM` (pack `fmap` (lookup "INTUIT_TOKEN" env))
                      `ap`    (pack `fmap` (lookup "INTUIT_SECRET" env))

------------
--TestData--
------------
trashEmailAccount :: (IsString a) => a
trashEmailAccount = "xvh221@sharklasers.com"

testEmail :: E.EmailAddress
testEmail = fromJust $ E.emailAddress trashEmailAccount

testLine :: Line
testLine = Line
  { lineId                    = Nothing
  , lineLineNum               = Nothing
  , lineDescription           = Nothing
  , lineAmount                = Just 100.0
  , lineLinkedTxn             = Nothing
  , lineDetailType            = "SalesItemLineDetail"
  , lineDescriptionLineDetail = Nothing
  , lineDiscountLineDetail    = Nothing
  , lineSalesItemLineDetail   = Just testSalesItemLineDetail
  , lineSubTotalLineDetail    = Nothing
  , lineCustomField           = Nothing
  }

testSalesItemLineDetail :: SalesItemLineDetail
testSalesItemLineDetail =
  SalesItemLineDetail
   { salesItemLineDetailItemRef         = Just testItemRef
   , salesItemLineDetailClassRef        = Nothing
   , salesItemLineDetailUnitPrice       = Nothing
   , salesItemLineDetailRatePercent     = Nothing
   , salesItemLineDetailPriceLevelRef   = Nothing
   , salesItemLineDetailMarkupInfo      = Nothing
   , salesItemLineDetailQty             = Nothing
   , salesItemLineDetailTaxCodeRef      = Nothing
   , salesItemLineDetailServiceData     = Nothing
   , salesItemLineDetailTaxInclusiveAmt = Nothing
  }


testItemRef :: Reference
testItemRef = Reference
  { referenceValue = "1"
  , referenceName  = Nothing
  , referenceType  = Nothing
  }

testItemRef2 :: Reference
testItemRef2 = Reference
  { referenceValue = "3"
  , referenceName  = Nothing
  , referenceType  = Nothing
  }


testCustomerRef :: CustomerRef
testCustomerRef  = Reference
  { referenceValue = "21"
  , referenceName  = Nothing
  , referenceType  = Nothing
  }

testItemIncomeAccountRef :: Reference
testItemIncomeAccountRef = Reference
  { referenceValue = "79"
  , referenceName  = Just "Sales of Product Income"
  , referenceType  = Nothing
  }

testItemExpenseAccountRef :: Reference
testItemExpenseAccountRef = Reference
  { referenceName  = Just "Cost of Goods Sold"
  , referenceValue = "80"
  , referenceType  = Nothing
  }

testItemAssetAccountRef :: Reference
testItemAssetAccountRef = Reference
  { referenceName  = Just "Inventory Asset"
  , referenceValue = "81"
  , referenceType  = Nothing
  }

makeTestParentRef :: Text -> Reference
makeTestParentRef parentId = Reference
  { referenceValue = parentId
  , referenceName = Nothing
  , referenceType  = Nothing
  }

makeTestBundleGroupDetail :: IO ItemGroupDetail
makeTestBundleGroupDetail = do
  let testItemLine1 = ItemGroupLine
        { itemRef = (Just testItemRef)
        , itemQty = (Just 2)
        }
      testItemLine2 = ItemGroupLine
        { itemRef = (Just testItemRef2)
        , itemQty = (Just 4)
        }
  return $ ItemGroupDetail
    { itemGroupLine = (Just [testItemLine1, testItemLine2]) }

-- The following generated name are always going to be safe for QBText fields
getTestItemName :: IO QBText
getTestItemName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  case (filterTextForQB $ T.pack $ "testItemName" ++ show arbInt) of
    Right eitherName -> return eitherName

getTestBundleName :: IO QBText
getTestBundleName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  case (filterTextForQB $ T.pack $ "testBundleName" ++ show arbInt) of
    Right eitherName -> return eitherName

getTestCategoryName :: IO QBText
getTestCategoryName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  case (filterTextForQB $ T.pack $ "testCategoryName" ++ show arbInt) of
    Right eitherName -> return eitherName

getTestCustomerName :: IO QBText
getTestCustomerName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  case (filterTextForQB $ T.pack $ "testCustomerName" ++ show arbInt) of
    Right eitherName -> return eitherName

makeTestCustomer :: IO Customer
makeTestCustomer = do
  customerName' <- getTestCustomerName
  return $ Customer
    { customerId = Nothing                      -- Maybe QBText
    , customerSyncToken = Nothing               -- Maybe SyncToken
    , customerMetaData = Nothing                -- Maybe ModificationMetaData
    , customerTitle = Nothing                   -- Maybe QBText -- def null
    , customerGivenName = Nothing               -- Maybe QBText -- max 25 def null
    , customerMiddleName = Nothing              -- Maybe QBText -- max 25, def null
    , customerFamilyName = Nothing              -- Maybe QBText -- max 25, def null
    , customerSuffix = Nothing                  -- Maybe QBText -- max 10, def null
    , customerFullyQualifiedName = Nothing      -- Maybe QBText
    , customerCompanyName = Nothing             -- Maybe QBText -- max 50, def null
    , customerDisplayName = customerName'       -- QBText -- unique
    , customerPrintOnCheckName = Nothing        -- Maybe QBText -- max 100
    , customerActive = Nothing                  -- Maybe Bool -- def true
    , customerPrimaryPhone = Nothing            -- Maybe TelephoneNumber
    , customerAlternatePhone = Nothing          -- Maybe TelephoneNumber
    , customerMobile = Nothing                  -- Maybe TelephoneNumber
    , customerFax = Nothing                     -- Maybe TelephoneNumber
    , customerPrimaryEmailAddress = Nothing     -- Maybe EmailAddress
    , customerWebAddr = Nothing                 -- Maybe WebSiteAddress
    , customerDefaultTaxCodeRef = Nothing       -- Maybe TaxCodeRef
    , customerTaxable = Nothing                 -- Maybe Bool
    , customerBillAddr = Nothing                -- Maybe BillAddr
    , customerShipAddr = Nothing                -- Maybe ShipAddr
    , customerNotes = Nothing                   -- Maybe QBText -- max 2000
    , customerJob = Nothing                     -- Maybe Bool -- def false or null
    , customerBillWithParent = Nothing          -- Maybe Bool -- def false or null
    , customerParentRef = Nothing               -- Maybe CustomerRef
    , customerLevel = Nothing                   -- Maybe Int -- def 0, up to 5
    , customerSalesTermRef = Nothing            -- Maybe SalesTermRef
    , customerPaymentMethodRef = Nothing        -- Maybe Reference
    , customerBalance = Nothing                 -- Maybe Double
    , customerOpenBalanceDate = Nothing         -- Maybe QBText
    , customerBalanceWithJobs = Nothing         -- Maybe Double
    , customerCurrencyRef = Nothing             -- Maybe CurrencyRef
    , customerPreferredDeliveryMethod = Nothing -- Maybe QBText
    , customerResaleNum = Nothing               -- Maybe QBText -- max 15
    }

makeTestItem :: IO Item
makeTestItem = do
  itemName' <- getTestItemName
  return $ Item Nothing                 -- Id
       (Just $ SyncToken "0")           -- Sync Token
       Nothing                          -- Metadata
       itemName'                         -- Name
       Nothing                          -- Description
       (Just False)                     -- Active
       Nothing                          -- Sub Item
       Nothing                          -- Parent Ref
       (Just 0)                         -- Item Level
       Nothing                          -- FullyQualifiedName
       Nothing                          -- Taxable
       Nothing                          -- Sales Tax Included
       (Just 0)                         -- Unit Price
       (Just "Inventory")               -- Type
       (Just testItemIncomeAccountRef)  -- IncomeAccountRef
       (Just "Purchase Desc")           -- Purchase Description
       Nothing                          -- Purchase Tax Included
       (Just 0)                         -- Purchase Cost
       (Just testItemExpenseAccountRef) -- Item Expense Account Ref
       (Just testItemAssetAccountRef)   -- Asset Account Ref
       (Just True)                      -- TrackQtyOnHand
       (Just 10)                        -- QtyOnHand
       Nothing                          -- SalesTaxCodeRef
       Nothing                          -- PurchaseTaxCodeRef
       (Just "2015-01-01")              -- InvStartDate

makeTestBundle :: IO Bundle
makeTestBundle = do
  bundleName' <- getTestBundleName
  testGroupDetail <- makeTestBundleGroupDetail
  return $ Bundle Nothing              -- Id
       (Just $ SyncToken "0")          -- SyncToken
       Nothing                         -- Metadata
       bundleName'                      -- Name
       Nothing                         -- SKU
       (Just True)                     -- Active
       Nothing                         -- Description
       Nothing                         -- Fully Qualified Name
       Nothing                         -- Taxable
       Nothing                         -- Unit Price
       (Just "Group")                  -- Type
       Nothing                         -- PurchaseCost
       (Just True)                     -- Print Group Items
       (Just testGroupDetail)          -- Group Detail

makeTestCategory :: IO Category
makeTestCategory = do
  categoryName' <- getTestCategoryName
  return $ Category Nothing            -- Id
       (Just $ SyncToken "0")          -- SyncToken
       Nothing                         -- Metadata
       categoryName'                    -- Name
       (Just True)                     -- Active
       Nothing                         -- SubItem
       Nothing                         -- ParentRef
       Nothing                         -- Level
       Nothing                         -- Fully Qualified Name
       (Just "Category")               -- Type

makeTestCategory2 :: Text -> IO Category
makeTestCategory2 parentId = do
  categoryName <- getTestCategoryName
  return $ Category
       Nothing                             -- Id
       (Just $ SyncToken "0")              -- SyncToken
       Nothing                             -- Metadata
       categoryName                        -- Name
       (Just True)                         -- Active
       (Just True)                         -- SubItem
       (Just (makeTestParentRef parentId)) -- ParentRef
       (Just 1)                            -- Level
       Nothing                             -- Fully Qualified Name
       (Just "Category")                   -- Type

testInvoice :: Invoice
testInvoice =
  Invoice Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          [testLine]
          Nothing
          testCustomerRef
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just $ EmailAddress trashEmailAccount)
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing

