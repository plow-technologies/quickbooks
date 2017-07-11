{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import QuickBooks
import Test.QuickCheck.Arbitrary       (arbitrary)
import Test.QuickCheck.Gen             (generate)

import           Control.Monad         (ap, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (fromJust)
import           Data.String
import qualified Data.Text             as T
import           Data.Text             (Text)
import           QuickBooks.Types
import           QuickBooks.QBText
import           System.Environment    (getEnvironment)
import qualified Text.Email.Validate   as E (EmailAddress, emailAddress)

main :: IO ()
main = do
    maybeTestOAuthToken <- lookupTestOAuthTokenFromEnv
    let oAuthToken' = maybe (error "") id maybeTestOAuthToken
    defaultMain $ tests oAuthToken'

tests :: OAuthToken -> TestTree
tests tok = testGroup "tests" [ testCase "Query Customer" $ queryCustomerTest tok
                              , testCase "Query Bundle" $ queryBundleTest tok
                              , testCase "Read Bundle" $ readBundleTest tok
                              , testCase "Query Category" $ queryCategoryTest tok
                              , testCase "Create Category" $ createCategoryTest tok
                              , testCase "Read Category" $ readCategoryTest tok
                              , testCase "Update Category" $ updateCategoryTest tok
                              , testCase "Delete Category" $ deleteCategoryTest tok
                              , testCase "Query Item" $ queryItemTest tok
                              , testCase "Query Count Items" $ queryCountTest tok
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

---- Query Customer ----
queryCustomerTest :: OAuthToken -> Assertion
queryCustomerTest oAuthToken = do
  eitherQueryCustomer <-
      queryCustomer oAuthToken "Rondonuwu Fruit and Vegi"
  case eitherQueryCustomer of
    Left _ ->
      assertEither "Faild to query customer" eitherQueryCustomer
    Right (QuickBooksCustomerResponse (customer:_)) -> do
      let (Right existingId) = filterTextForQB "21"
      assertBool (show $ customerId customer) (customerId customer == Just existingId)

-----------------
-- Item CRUD-Q --
-----------------

---- Create Item ----
createItemTest :: OAuthToken -> Assertion
createItemTest oAuthToken = do
  testItem <- makeTestItem
  resp <- createItem oAuthToken testItem
  case resp of
    Left err -> assertEither ("My custom error message: " ++ err) resp
    Right (QuickBooksItemResponse (item:_)) -> do
      deleteItem oAuthToken item
      assertEither "I created an item!" resp

---- Read Item ----
readItemTest :: OAuthToken -> Assertion
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
updateItemTest :: OAuthToken -> Assertion
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
deleteItemTest :: OAuthToken -> Assertion
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
queryItemTest :: OAuthToken -> Assertion
queryItemTest oAuthToken = do
  eitherQueryItem <- queryItem oAuthToken "Hours"
  case eitherQueryItem of
    Left _ ->
      assertEither "Failed to query item" eitherQueryItem
    Right (QuickBooksItemResponse (item:_)) -> do
      let (Right existingId) = filterTextForQB "2"
      assertBool (show $ itemId item) (itemId item == Just existingId)

---- Query Max Item ----
queryMaxItemTest :: OAuthToken -> Assertion
queryMaxItemTest oAuthToken = do
  eitherQueryItems <- queryMaxItemsFrom oAuthToken 1
  case eitherQueryItems of
    Left _ ->
      assertEither "Failed to query item" eitherQueryItems
    Right (QuickBooksItemResponse (item:_)) -> do
      assertEither "Queried for max size" eitherQueryItems

queryCountTest :: OAuthToken -> Assertion
queryCountTest oAuthToken = do
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
readBundleTest :: OAuthToken -> Assertion
readBundleTest oAuthToken = do
  let (Right existingId) = filterTextForQB "208"
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
queryBundleTest :: OAuthToken -> Assertion
queryBundleTest oAuthToken = do
  let (Right existingId) = filterTextForQB "208"
  let existingBundleId = existingId -- this MUST be created in QB Online for the test to pass
                            -- Replace the number the id for the existing bundle
                            -- It can be obtained by querying the bundle name below
                            -- Or through the API Explorer at https://developer.intuit.com/v2/apiexplorer?apiname=V3QBO#?id=Item
                            -- With select * from item where Type='Group'
  eitherQueryBundle <- queryBundle oAuthToken "Bundle 1"
  case eitherQueryBundle of
    Right (QuickBooksBundleResponse (bundle:_)) -> do
      let bundleId' = fromJust (bundleId bundle)
      assertBool (show $ bundleId') (bundleId' == existingBundleId)
    _ ->
      assertEither "Failed to query bundle" eitherQueryBundle




---------------------
-- Category CRUD-Q --
---------------------

---- Create Category ----
createCategoryTest :: OAuthToken -> Assertion
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
readCategoryTest :: OAuthToken -> Assertion
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
updateCategoryTest :: OAuthToken -> Assertion
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
deleteCategoryTest :: OAuthToken -> Assertion
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
queryCategoryTest :: OAuthToken -> Assertion
queryCategoryTest oAuthToken = do
  eitherQueryCategory <- queryCategory oAuthToken "Cat 1"
  case eitherQueryCategory of
    Left _ ->
      assertEither "Failed to query category" eitherQueryCategory
    Right (QuickBooksCategoryResponse (category:_)) -> do
      let (Right existingId) = filterTextForQB "91"
      assertBool (show $ categoryId category) (categoryId category == (Just existingId))





---- Invoice CRUD-Email ----
createInvoiceTest :: OAuthToken -> Assertion
createInvoiceTest oAuthToken = do
  resp <- createInvoice oAuthToken testInvoice
  case resp of
    Left err -> assertEither ("My custom error message: " ++ err) resp
    Right (QuickBooksInvoiceResponse invoice) -> do
     deleteInvoice oAuthToken (fromJust (invoiceId invoice)) (fromJust (invoiceSyncToken invoice))
     assertEither "I created an invoice!" resp

readInvoiceTest :: OAuthToken -> Assertion
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

updateInvoiceTest :: OAuthToken -> Assertion
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

deleteInvoiceTest :: OAuthToken -> Assertion
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

emailInvoiceTest :: OAuthToken -> Assertion
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

getTestItemName :: IO QBText
getTestItemName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  let (Right eitherName) = filterTextForQB $ T.pack $ "testItemName" ++ show arbInt
  return eitherName 

getTestBundleName :: IO QBText
getTestBundleName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  let (Right eitherName) = filterTextForQB $ T.pack $ "testBundleName" ++ show arbInt
  return eitherName 

getTestCategoryName :: IO QBText
getTestCategoryName = do
  arbInt <- generate (choose (0, 100000000000000000) :: Gen Int)
  -- Ignoring case check (Should always be a QBText)
  let (Right eitherName) = filterTextForQB $ T.pack $ "testCategoryName" ++ show arbInt
  return eitherName 

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

