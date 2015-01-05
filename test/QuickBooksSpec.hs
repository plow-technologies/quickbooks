{-# LANGUAGE OverloadedStrings #-}

module QuickBooksSpec (spec) where

import Control.Monad (void)
import QuickBooks
import Test.Hspec
import QuickBooks.Types
import Data
import Data.Maybe

spec :: Spec
spec = quickBooksAPISpec

quickBooksAPISpec :: Spec
quickBooksAPISpec = do
  describe "QuickBooks API Binding" $ do
    it "queries quickbooks to create a new invoice."    createInvoiceTest
    it "queries an invoice given an invoice Identifier" readInvoiceTest
    it "queries quickbooks to update and invoice."      updateInvoiceTest
    it "queries quickbooks to delete and invoice."      deleteInvoiceTest
    it "gets temporary tokens." getTempTokensTest


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

