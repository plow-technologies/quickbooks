{-# LANGUAGE OverloadedStrings #-}

module QuickBooksSpec (spec) where

import Control.Monad (void)
import QuickBooks
import Test.Hspec
import QuickBooks.Types
import System.Environment
import Data
import Data.Maybe

spec :: Spec
spec = quickBooksAPISpec

quickBooksAPISpec :: Spec
quickBooksAPISpec = do
  describe "QuickBooks API" $ do
    it "queries quickbooks to create a new invoice."    createInvoiceTest
    it "queries an invoice given an invoice Identifier" readInvoiceTest
    it "queries quickbooks to update and invoice."      updateInvoiceTest
    it "queries quickbooks to delete and invoice."      deleteInvoiceTest
    it "gets temporary tokens."                         getTempTokensTest

setTestEnv :: IO ()
setTestEnv = do
 setEnv "INTUIT_COMPANY_ID"      "1315190090"
 setEnv "INTUIT_CONSUMER_KEY"    "qyprdJsVJQN726R7DY4QW7S9V5YUze"
 setEnv "INTUIT_CONSUMER_SECRET" "c1B6XtDtn8kFjrqRBhHovu7IlpX59sHCPqJFXvMi"
 setEnv "INTUIT_TOKEN"           "lvprdeMmQMNWSSPzSkF65TGhC3C6NYSGiiDFuNB3q4Szs6TC"
 setEnv "INTUIT_SECRET"          "QOLKz5xg0wjGsdiqUbRE7wLZgrgmcs2X7Zy9JFpl"
 setEnv "INTUIT_HOSTNAME"        "sandbox-quickbooks.api.intuit.com"

getTempTokensTest :: Expectation
getTempTokensTest = do
  setTestEnv
  temporaryTokens <- getTempTokens "localhost"
  case temporaryTokens of
    Left err -> print err
    Right _  -> return ()

createInvoiceTest :: Expectation
createInvoiceTest = do
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) ->
      do  void $ deleteInvoice (fromJust (invoiceId inv))
                               (fromJust (invoiceSyncToken inv))
          
readInvoiceTest :: Expectation
readInvoiceTest = do
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) -> do
      resp <- readInvoice (fromJust (invoiceId inv))  
      case resp of
        Left err -> print err
        Right _ -> void $ deleteInvoice (fromJust (invoiceId inv)) 
                                        (fromJust (invoiceSyncToken inv))

updateInvoiceTest :: Expectation
updateInvoiceTest = do 
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) -> do
      resp <- updateInvoice inv
      case resp of
        Left err -> print err
        Right _ -> void $ deleteInvoice (fromJust (invoiceId inv))
                                        (fromJust (invoiceSyncToken inv))

deleteInvoiceTest :: Expectation
deleteInvoiceTest = do
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) ->
      do  
      del <- deleteInvoice (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))
      case del of 
        Left err -> print err 
        Right _ ->  return ()

