module QuickBooksSpec (spec) where

import QuickBooks
import Test.Hspec

spec :: Spec
spec = quickBooksAPISpec

quickBooksAPISpec :: Spec
quickBooksAPISpec = describe "createInvoice" $ do
  it "requests the creation of an invoice given and invoice and authorization credentials." createInvoiceTest

createInvoiceTest :: Bool
createInvoiceTest = undefined
