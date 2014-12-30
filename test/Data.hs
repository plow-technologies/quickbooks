{-# LANGUAGE OverloadedStrings #-}

module Data where

import QuickBooks.Types

testLine :: Line
testLine = Line
  { lineId                    = Nothing
  , lineLineNum               = Nothing
  , lineDescription           = Nothing
  , lineAmount                = 100.0
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
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing

