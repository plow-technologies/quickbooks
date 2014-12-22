{-# LANGUAGE GADTs #-}

module QuickBooks where

import System.Environment (getEnv)

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
  CreateInvoice :: Invoice -> QuickBooksRequest
  -- ReadInvoice   :: String -> QuickBooksRequest
  -- UpdateInvoice :: Invoice -> QuickBooksRequest
  -- DeleteInvoice :: String -> QuickBooksRequest

data APIConfig = APIConfig
  { companyId :: String
  , apiKey    :: String
  }

runQuickBooksRequest :: QuickBooksRequest -> IO ()
runQuickBooksRequest (CreateInvoice invoice) = undefined
-- runQuickBooksRequest _ = undefined
