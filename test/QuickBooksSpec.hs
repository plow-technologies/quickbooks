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
quickBooksAPISpec = do
  describe "QuickBooks API Binding" $ do
    it "queries quickbooks to create a new invoice."     createInvoiceTest
    it "queries an invoice given an invoice Identifier." readInvoiceTest
    it "queries quickbooks to update and invoice."       updateInvoiceTest
    it "queries quickbooks to delete and invoice."       deleteInvoiceTest
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
      do  void $ deleteInvoice (fromJust (invoiceId inv))
                               (fromJust (invoiceSyncToken inv))

readInvoiceTest :: Expectation
readInvoiceTest = do
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

sendInvoiceTest :: Expectation
sendInvoiceTest = invoiceTest sendInvoiceTest'

sendInvoiceWithoutMessageTest :: Expectation
sendInvoiceWithoutMessageTest = invoiceTest sendInvoiceWithoutMessageTest'

sendInvoiceWithoutMessageTest' :: Either String (QuickBooksResponse Invoice) -> Expectation
sendInvoiceWithoutMessageTest' (Left err) = error err
sendInvoiceWithoutMessageTest' (Right (QuickBooksInvoiceResponse inv)) = do
  let invId = fromJust (invoiceId inv)
  sendInvoiceResponse <- sendInvoice' invId
  case sendInvoiceResponse of
    Left err -> print err
    Right _ -> return ()

sendInvoiceTest' :: Either String (QuickBooksResponse Invoice) -> Expectation
sendInvoiceTest' (Left err) = error err
sendInvoiceTest' (Right (QuickBooksInvoiceResponse inv)) = do
  let invId = fromJust (invoiceId inv)
  sendInvoiceResponse <- sendInvoice invId testEmail
  case sendInvoiceResponse of
    Left err -> print err
    Right _ -> return ()

invoiceTest :: (Either String (QuickBooksResponse Invoice) -> Expectation) -> Expectation
invoiceTest test = bracket acquireInvoice
                           releaseInvoice
                           test
  where
    acquireInvoice = createInvoice testInvoice
    releaseInvoice (Left err) = error err
    releaseInvoice (Right (QuickBooksInvoiceResponse inv)) =
      void $ deleteInvoice (fromJust (invoiceId inv))
                           (fromJust (invoiceSyncToken inv))
