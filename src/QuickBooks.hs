{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE ImplicitParams #-}

module QuickBooks where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Network.Wreq
import System.Environment (getEnv)

data SalesItemLineDetail = SalesItemLineDetail
  deriving (Generic)

data DetailType = DetailType
  deriving (Generic)

data Line = Line
  { lineId                  :: !(Maybe String)
  , lineAmount              :: !Double
  , lineDetailType          :: !DetailType
  , lineSalesItemLineDetail :: !SalesItemLineDetail
  }
  deriving (Generic)

data CustomerRef = CustomerRef
  { customerRefValue :: !String
  }
  deriving (Generic)

data Invoice = Invoice
  { invoiceId          :: !(Maybe String)
  , invoiceLines       :: ![Line]
  , invoiceCustomerRef :: !CustomerRef
  }
  deriving (Generic)

instance ToJSON CustomerRef
instance ToJSON DetailType
instance ToJSON Invoice
instance ToJSON Line
instance ToJSON SalesItemLineDetail

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
