{-# LANGUAGE GADTs           #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TemplateHaskell #-}

module QuickBooks where

import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Network.Wreq
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

$(deriveJSON defaultOptions{fieldLabelModifier = drop 11} ''CustomerRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''DetailType)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 7} ''Invoice)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Line)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''SalesItemLineDetail)

data APIKey = APIKey

data InvoiceOperation where
  CreateInvoice :: Invoice -> InvoiceOperation
  -- ReadInvoice   :: String -> InvoiceOperation
  -- UpdateInvoice :: Invoice -> InvoiceOperation
  -- DeleteInvoice :: String -> InvoiceOperation

data APIConfig = APIConfig
  { companyId :: String
  , apiKey    :: String
  }

runInvoiceOperation :: (?apiConfig :: APIConfig) => InvoiceOperation -> IO ()
runInvoiceOperation (CreateInvoice invoice) = do
  post createInvoiceURL (toJSON invoice)
  return ()

createInvoiceURL :: (?apiConfig :: APIConfig) => String
createInvoiceURL = "/v3/company/" <> companyId ?apiConfig <> "/invoice"
