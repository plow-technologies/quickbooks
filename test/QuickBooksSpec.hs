{-# LANGUAGE OverloadedStrings #-}

module QuickBooksSpec (spec) where

import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Control.Monad (void, liftM, ap)
import QuickBooks
import Test.Hspec
import QuickBooks.Types hiding (EmailAddress, emailAddress)
import Data
import Data.Maybe
import System.IO.Unsafe
import System.Environment (getEnvironment)

quickBooksAPISpec :: OAuthToken -> Spec
quickBooksAPISpec tok =
  describe "QuickBooks API Binding" $ do
    it "queries quickbooks to create a new invoice."     (createInvoiceTest tok)
    it "queries an invoice given an invoice identifier." (readInvoiceTest tok)
    it "queries quickbooks to update an invoice."        (updateInvoiceTest tok)
    it "queries quickbooks to delete an invoice."        (deleteInvoiceTest tok)
    it "gets temporary tokens."                          getTempTokensTest
    it "emails invoices given an address."               (sendInvoiceTest tok)

spec :: Spec
spec = do
  let maybeTestToken = unsafePerformIO $ lookupTestOAuthTokenFromEnv
  maybe (error tokenLookupError) quickBooksAPISpec maybeTestToken
  where
   tokenLookupError = "Error looking up credentials from environment. \
                      \Please ensure that the environment variables \ 
                      \'INTUIT_TOKEN' 'INTUIT_SECRET' are set in order \ 
                      \to run tests against your sandbox."

lookupTestOAuthTokenFromEnv :: IO (Maybe OAuthToken)
lookupTestOAuthTokenFromEnv = do
  env <- getEnvironment
  return $ OAuthToken `liftM` (pack `fmap` (lookup "INTUIT_TOKEN" env))
                      `ap`    (pack `fmap` (lookup "INTUIT_SECRET" env))

getTempTokensTest :: Expectation
getTempTokensTest = do
  temporaryTokens <- getTempTokens "localhost"
  case temporaryTokens of
    Left err -> print err
    Right _  -> return ()

createInvoiceTest :: OAuthToken -> Expectation
createInvoiceTest testOAuthToken = do
  quickBooksInvoiceResponse <- createInvoice testOAuthToken testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) ->
      void $ deleteInvoice testOAuthToken
                           (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))

readInvoiceTest :: OAuthToken -> Expectation
readInvoiceTest testOAuthToken = 
  void $ invoiceTest testOAuthToken (readInvoice testOAuthToken . fromJust . invoiceId)

updateInvoiceTest :: OAuthToken -> Expectation
updateInvoiceTest testOAuthToken = void $ invoiceTest testOAuthToken (updateInvoice testOAuthToken)

deleteInvoiceTest :: OAuthToken -> Expectation
deleteInvoiceTest testOAuthToken = do
  quickBooksInvoiceResponse <- createInvoice testOAuthToken testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) ->
      do
      del <- deleteInvoice testOAuthToken
                           (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))
      case del of
        Left err -> print err
        Right _ ->  return ()

sendInvoiceTest :: OAuthToken -> Expectation
sendInvoiceTest tok = invoiceTest tok (sendInvoiceTest' tok)

sendInvoiceTest' :: OAuthToken -> Invoice -> Expectation
sendInvoiceTest' testOAuthToken inv = do
  let invId = fromJust (invoiceId inv)
  sendInvoiceResponse <- sendInvoice testOAuthToken invId testEmail
  either print (return . return ()) sendInvoiceResponse

invoiceTest :: OAuthToken -> (Invoice -> IO c) -> IO c
invoiceTest testOAuthToken fn = bracket acquireInvoice releaseInvoice fn
  where
    acquireInvoice = do 
     resp <- createInvoice testOAuthToken testInvoice
     let (QuickBooksInvoiceResponse inv) = either error id resp
     return inv

    releaseInvoice inv = 
      void $ deleteInvoice testOAuthToken
                           (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))
