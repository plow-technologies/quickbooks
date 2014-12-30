{-# LANGUAGE OverloadedStrings #-}

module QuickBooksSpec (spec) where

import QuickBooks
import Test.Hspec
import QuickBooks.Types
import System.Environment
import Data
import Data.Maybe

spec :: Spec
spec = quickBooksAPISpec

quickBooksAPISpec :: Spec
quickBooksAPISpec = describe "readInvoice" $ do
  it "queries an invoice given an invoice Identifier" readInvoiceTest
  it "queries quickbooks to create a new invoice." createInvoiceTest

setTestEnv :: IO ()
setTestEnv = do
 setEnv "INTUIT_COMPANY_ID"      "1315190090"
 setEnv "INTUIT_CONSUMER_KEY"    "qyprdJsVJQN726R7DY4QW7S9V5YUze"
 setEnv "INTUIT_CONSUMER_SECRET" "c1B6XtDtn8kFjrqRBhHovu7IlpX59sHCPqJFXvMi"
 setEnv "INTUIT_TOKEN"           "lvprdeMmQMNWSSPzSkF65TGhC3C6NYSGiiDFuNB3q4Szs6TC"
 setEnv "INTUIT_SECRET"          "QOLKz5xg0wjGsdiqUbRE7wLZgrgmcs2X7Zy9JFpl"
 setEnv "INTUIT_HOSTNAME"        "sandbox-quickbooks.api.intuit.com"

createInvoiceTest :: Expectation
createInvoiceTest = do
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) ->
      do  deleteInvoice (fromJust (invoiceId inv))
                        (fromJust (invoiceSyncToken inv))
          return ()

readInvoiceTest :: Expectation
readInvoiceTest = do
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) -> do
      quickBooksInvoiceResponse'  <- readInvoice (fromJust (invoiceId inv))  
      quickBooksInvioceResponse'' <- deleteInvoice (fromJust (invoiceId inv)) 
                                                   (fromJust (invoiceSyncToken inv))
      return ()

deleteInvoiceTest :: Expectation
deleteInvoiceTest = do
  setTestEnv
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print ("Creat error: " ++ err)
    Right (QuickBooksInvoiceResponse inv) ->
      do  
      del <- deleteInvoice (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))
      case del of 
        Left err -> print ("Delete error: " ++ err)
        Right _ ->  return ()

