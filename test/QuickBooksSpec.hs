{-# LANGUAGE OverloadedStrings #-}

module QuickBooksSpec (spec) where

import QuickBooks
import Test.Hspec
import QuickBooks.Types
import System.Environment

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
  invoice <- readInvoice testInvoiceId
  case invoice of
    Left err ->  print err
    Right (QuickBooksInvoiceResponse invc) -> do
      let invc' = invc{invoiceId = Nothing}
      resp <- createInvoice invc'
      case  resp of
        Left err -> print err
        Right _  -> return ()
  where testInvoiceId = InvoiceId "148"

readInvoiceTest :: Expectation
readInvoiceTest = do
  setTestEnv
  invoice <- readInvoice testInvoiceId
  case invoice of
    Left err -> print err
    Right _  -> return ()
  where testInvoiceId = InvoiceId "148"
