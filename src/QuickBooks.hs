{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

module QuickBooks where

import Control.Applicative ((<$>))
import Data.Aeson
import GHC.Generics
import Data.Text(Text)
import Network.Wreq
import Network.HTTP.Types.URI
import Data.ByteString.Char8
import Blaze.ByteString.Builder

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Generic)

newtype LineId    = LineId {unLineId :: Text}
  deriving (Generic)

newtype CustomerRefValue = CustomerRefValue {unCustomerRefValue :: Text}
  deriving (Generic)

data SalesItemLineDetail = SalesItemLineDetail
  deriving (Generic)

data DetailType = DetailType
  deriving (Generic)

data Line = Line
  { lineId                  :: !(Maybe LineId)
  , lineAmount              :: !Double
  , lineDetailType          :: !DetailType
  , lineSalesItemLineDetail :: !SalesItemLineDetail
  } deriving (Generic)

data CustomerRef = CustomerRef
  { customerRefValue :: !String
  } deriving (Generic)

data Invoice = Invoice
  { invoiceId          :: !(Maybe InvoiceId)
  , invoiceLines       :: ![Line]
  , invoiceCustomerRef :: !CustomerRef
  } deriving (Generic)

instance ToJSON LineId
instance ToJSON InvoiceId
instance ToJSON CustomerRefValue
instance ToJSON SalesItemLineDetail
instance ToJSON DetailType
instance ToJSON Line
instance ToJSON CustomerRef
instance ToJSON Invoice

instance FromJSON LineId
instance FromJSON InvoiceId
instance FromJSON CustomerRefValue
instance FromJSON SalesItemLineDetail
instance FromJSON DetailType
instance FromJSON Line
instance FromJSON CustomerRef
instance FromJSON Invoice

data APIKey = APIKey

data family QuickBooksResponse a
data instance QuickBooksResponse Invoice = QuickBooksInvoiceResponse Invoice

instance FromJSON (QuickBooksResponse Invoice) where
  parseJSON (Object o) = QuickBooksInvoiceResponse <$> o .: "invoice"
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"

data QuickBooksRequest a where
  CreateInvoice :: Invoice -> QuickBooksQuery Invoice
  ReadInvoice   :: InvoiceId -> QuickBooksQuery Invoice 
--  UpdateInvoice :: Invoice ->  QuickBooksRequest Invoice
  DeleteInvoice :: InvoiceId  -> QuickBooksQuery Invoice

type QuickBooksQuery a = QuickBooksRequest (QuickBooksResponse a)

data APIConfig = APIConfig
  { companyId :: !Text
  , apiKey    :: !Text
  , hostname  :: !Text
  }

evalQuickBooksQuery :: (?apiConfig :: APIConfig) => QuickBooksQuery a -> IO a
evalQuickBooksQuery rq@(CreateInvoice invoice) = undefined =<< post (requestURI rq) (toJSON invoice)
evalQuickBooksQuery rq@(ReadInvoice _ )        = undefined =<< get $ requestURI rq
evalQuickBooksQuery rq@(DeleteInvoice _ )      = undefined =<< delete $ requestURI rq

requestURI :: (?apiConfig :: APIConfig) => QuickBooksRequest a -> String
requestURI (CreateInvoice _)                  = encodeURI ["v3", "company", companyId ?apiConfig, "company", "invoice"]
requestURI (ReadInvoice (unInvoiceId -> t))   = encodeURI ["v3", "company", companyId ?apiConfig, "company", t]
requestURI (DeleteInvoice (unInvoiceId -> t)) = encodeURI ["v3", "company", companyId ?apiConfig, "company", t]

encodeURI :: (?apiConfig :: APIConfig) => [Text] -> String
encodeURI = unpack . toByteString . encodePathSegments . (:)(hostname ?apiConfig)
