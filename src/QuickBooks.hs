{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TemplateHaskell    #-}

module QuickBooks where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Data.Text(Text)
import Network.Wreq
import Network.HTTP.Types.URI
import qualified Data.ByteString.Char8 as BSC
import Blaze.ByteString.Builder

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Generic)

newtype LineId    = LineId {unLineId :: Text}
  deriving (Generic)

newtype CustomerRefValue = CustomerRefValue {unCustomerRefValue :: Text}
  deriving (Generic)

data SalesItemLineDetail = SalesItemLineDetail

data DetailType = DetailType

data Line = Line
  { lineId                  :: !(Maybe LineId)
  , lineAmount              :: !Double
  , lineDetailType          :: !DetailType
  , lineSalesItemLineDetail :: !SalesItemLineDetail
  }

data ClassRef = ClassRef
  { classRefName  :: !(Maybe String)
  , classRefValue :: !String
  }

data CurrencyRef = CurrencyRef
  { currencyRefName  :: !(Maybe String)
  , currencyRefValue :: !String
  }

data CustomerRef = CustomerRef
  { customerRefValue :: !String
  }

data DepartmentRef = DepartmentRef
  { departmentRefValue :: !String
  }

data MetaData = MetaData
  { metaDataCreateTime      :: !String
  , metaDataLastUpdatedTime :: !String
  }

data PhysicalAddress = PhysicalAddress
  { physicalAddressId                     :: !String
  , physicalAddressLine1                  :: !String
  , physicalAddressLine2                  :: !String
  , physicalAddressLine3                  :: !String
  , physicalAddressLine4                  :: !String
  , physicalAddressLine5                  :: !String
  , physicalAddressCity                   :: !String
  , physicalAddressCountry                :: !String
  , physicalAddressCountrySubDivisionCode :: !String
  , physicalAddressPostalCode             :: !String
  , physicalAddressNote                   :: !(Maybe String)
  , physicalAddressLat                    :: !String
  , physicalAddressLong                   :: !String
  }

data Invoice = Invoice
  { invoiceId               :: !(Maybe InvoiceId)
  , invoiceLines            :: ![Line]
  , invoiceCustomerRef      :: !CustomerRef
  , invoiceDeposit          :: !(Maybe Double)
  , invoiceAllowIPNPayment  :: !(Maybe Bool)
  , invoiceDomain           :: !(Maybe Text)
  , invoiceSparse           :: !(Maybe Bool)
  , invoiceSyncToken        :: !(Maybe String)
  , invoiceMetaData         :: !(Maybe MetaData)

  , invoiceDocNumber        :: !(Maybe String)
  , invoiceTxnDate          :: !(Maybe String)
  , invoiceDepartmentRef    :: !(Maybe DepartmentRef)
  , invoiceCurrencyRef      :: !(Maybe CurrencyRef)
  , invoicePrivateNote      :: !(Maybe String)

  , invoiceCustomerMemo     :: !(Maybe String)
  , invoiceBillAddr         :: !(Maybe PhysicalAddress)
  , invoiceShipAddr         :: !(Maybe PhysicalAddress)
  , invoiceClassRef         :: !(Maybe ClassRef)
  }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 8} ''ClassRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 11} ''CurrencyRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 11} ''CustomerRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''DepartmentRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''DetailType)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 7} ''Invoice)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9} ''InvoiceId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Line)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''LineId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 8} ''MetaData)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 15} ''PhysicalAddress)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''SalesItemLineDetail)

data APIKey = APIKey

data family QuickBooksResponse a
data instance QuickBooksResponse Invoice = QuickBooksInvoiceResponse { quickBooksResponseInvoice :: Invoice }

instance FromJSON (QuickBooksResponse Invoice) where
  parseJSON (Object o) = QuickBooksInvoiceResponse <$> o .: "invoice"
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"

data QuickBooksRequest a where
  CreateInvoice :: Invoice -> QuickBooksQuery Invoice
  ReadInvoice   :: InvoiceId -> QuickBooksQuery Invoice 
-- UpdateInvoice :: Invoice ->  QuickBooksRequest Invoice
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
encodeURI = BSC.unpack . toByteString . encodePathSegments . (:)(hostname ?apiConfig)
