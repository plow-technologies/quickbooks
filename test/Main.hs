{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import QuickBooks

import           Control.Monad         (ap, liftM)
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (fromJust)
import           Data.String
import           QuickBooks.Types
import           System.Environment    (getEnvironment, getEnv)
import qualified Text.Email.Validate   as E (EmailAddress, emailAddress)

main :: IO ()
main = do
    maybeTestOAuthToken <- lookupTestOAuthTokenFromEnv
    let oAuthToken' = maybe (error "") id maybeTestOAuthToken
    defaultMain $ tests oAuthToken'

tests :: OAuthToken -> TestTree
tests tok = testGroup "tests" [ testCase "Query Customer" $ queryCustomerTest tok
                              , testCase "Query Item" $ queryItemTest tok
                              , testCase "Create Invoice" $ createInvoiceTest tok
                              , testCase "Read Invoice" $ readInvoiceTest tok
                              , testCase "Update Invoice" $ updateInvoiceTest tok
                              , testCase "Delete Invoice" $ deleteInvoiceTest tok
                              , testCase "Email Invoice" $ emailInvoiceTest tok
                              , testCase "Temp Tokens" $ tempTokenTest]

---------
--Tests--
---------
queryCustomerTest :: OAuthToken -> Assertion
queryCustomerTest oAuthToken = do
  eitherQueryCustomer <-
      queryCustomer oAuthToken "Rondonuwu Fruit and Vegi"
  case eitherQueryCustomer of
    Right (QuickBooksCustomerResponse (customer:_)) ->
      assertBool (show $ customerId customer) (customerId customer == Just "21")
    _ ->
      assertEither "Faild to query customer" eitherQueryCustomer

queryItemTest :: OAuthToken -> Assertion
queryItemTest oAuthToken = do 
  eitherQueryItem <- queryItem oAuthToken "Hours"
  case eitherQueryItem of
    Right (QuickBooksItemResponse (item:_)) ->
      assertBool (show $ itemId item) (itemId item == Just "2")
    _ ->
      assertEither "Failed to query item" eitherQueryItem

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


testCustomerRef :: CustomerRef
testCustomerRef  = Reference
  { referenceValue = "21"
  , referenceName  = Nothing
  , referenceType  = Nothing
  }

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

