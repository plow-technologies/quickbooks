{-# LANGUAGE GADTs          #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveGeneric  #-}

module QuickBooks where

import System.Environment (getEnv)
import Network.Wreq
import Data.Monoid
import Data.Aeson
import GHC.Generics

data SalesItemLineDetail = SalesItemLineDetail
  deriving (Generic)

data DetailType = DetailType
  deriving (Generic)

data Line = Line
  { lineId                  :: !(Maybe String)
  , lineAmount              :: !Double
  , lineDetailType          :: !DetailType
  , lineSalesItemLineDetail :: !SalesItemLineDetail
  } deriving (Generic)

data CustomerRef = CustomerRef
  { customerRefValue :: !String
  } deriving (Generic)

data Invoice = Invoice
  { invoiceId          :: !(Maybe String)
  , invoiceLines       :: ![Line]
  , invoiceCustomerRef :: !CustomerRef
  } deriving (Generic)

instance ToJSON SalesItemLineDetail
instance ToJSON DetailType
instance ToJSON Line
instance ToJSON CustomerRef
instance ToJSON Invoice

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

runQuickBooksRequest :: (?apiConfig :: APIConfig) => QuickBooksRequest -> IO ()
runQuickBooksRequest (CreateInvoice invoice) = do 
  post createInvoiceURL (toJSON invoice)
  return ()

createInvoiceURL :: (?apiConfig :: APIConfig) => String
createInvoiceURL = "/v3/company/" <> companyId ?apiConfig <> "/invoice"


