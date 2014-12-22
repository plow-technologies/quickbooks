{-# LANGUAGE GADTs #-}

module QuickBooks where

data SalesItemLineDetail = SalesItemLineDetail

data DetailType = DetailType

data Line = Line
  { lineId                  :: !(Maybe String)
  , lineAmount              :: !Double
  , lineDetailType          :: !DetailType
  , lineSalesItemLineDetail :: !SalesItemLineDetail
  }

data CustomerRef = CustomerRef
  { customerRefValue :: !String
  }

data Invoice = Invoice
  { invoiceId          :: !(Maybe String)
  , invoiceLines       :: ![Line]
  , invoiceCustomerRef :: !CustomerRef
  }

data APIKey = APIKey

data QuickBooksRequest where
  CreateInvoice :: Invoice -> APIKey -> QuickBooksRequest
  -- ReadInvoice   :: String -> APIKey -> QuickBooksRequest
  -- UpdateInvoice :: Invoice -> APIKey -> QuickBooksRequest
  -- DeleteInvoice :: String -> APIKey -> QuickBooksRequest

runQuickBooksRequest :: QuickBooksRequest -> IO ()
runQuickBooksRequest (CreateInvoice invoice key) = undefined
-- runQuickBooksRequest _ = undefined
