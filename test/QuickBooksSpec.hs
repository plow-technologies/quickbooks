{-# LANGUAGE OverloadedStrings #-}

module QuickBooksSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (void)
import QuickBooks
import Test.Hspec
import QuickBooks.Types hiding (EmailAddress, emailAddress)
import Data
import Data.Maybe

spec :: Spec
spec = quickBooksAPISpec

quickBooksAPISpec :: Spec
quickBooksAPISpec =
  describe "QuickBooks API Binding" $ do
    it "queries quickbooks to create a new invoice."     createInvoiceTest
    it "queries an invoice given an invoice identifier." readInvoiceTest
    it "queries quickbooks to update an invoice."        updateInvoiceTest
    it "queries quickbooks to delete an invoice."        deleteInvoiceTest
    it "gets temporary tokens."                          getTempTokensTest
    it "emails invoices given an address."               sendInvoiceTest
    it "emails invoice with emails supplied by invoice." sendInvoiceWithoutMessageTest

getTempTokensTest :: Expectation
getTempTokensTest = do
  temporaryTokens <- getTempTokens "localhost"
  case temporaryTokens of
    Left err -> print err
    Right _  -> return ()

createInvoiceTest :: Expectation
createInvoiceTest = do
  quickBooksInvoiceResponse <- createInvoice testInvoice
  case quickBooksInvoiceResponse of
    Left err -> print err
    Right (QuickBooksInvoiceResponse inv) ->
      void $ deleteInvoice (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))

readInvoiceTest :: Expectation
readInvoiceTest = 
  void $ invoiceTest (readInvoice . fromJust . invoiceId)

updateInvoiceTest :: Expectation
updateInvoiceTest = void $ invoiceTest updateInvoice

deleteInvoiceTest :: Expectation
deleteInvoiceTest = do
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

sendInvoiceWithoutMessageTest :: Expectation
sendInvoiceWithoutMessageTest = invoiceTest sendInvoiceWithoutMessageTest'

sendInvoiceWithoutMessageTest' :: Invoice -> Expectation
sendInvoiceWithoutMessageTest' inv = do
  let invId = fromJust (invoiceId inv)
  sendInvoiceResponse <- sendInvoice' invId
  either print (return . return ()) sendInvoiceResponse

sendInvoiceTest :: Expectation
sendInvoiceTest = invoiceTest sendInvoiceTest'

sendInvoiceTest' :: Invoice -> Expectation
sendInvoiceTest' inv = do
  let invId = fromJust (invoiceId inv)
  sendInvoiceResponse <- sendInvoice invId testEmail
  either print (return . return ()) sendInvoiceResponse

invoiceTest :: (Invoice -> IO c) -> IO c
invoiceTest = bracket acquireInvoice releaseInvoice
  where
    acquireInvoice = do 
     resp <- createInvoice testInvoice
     let (QuickBooksInvoiceResponse inv) = either error id resp
     return inv

    releaseInvoice inv = 
      void $ deleteInvoice (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))
