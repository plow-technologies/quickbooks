{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module QuickBooks where

import           Blaze.ByteString.Builder
import           Control.Applicative ((<$>))
import           Data.Aeson
import           Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import qualified Data.ByteString.Char8 as BSC
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.HTTP.Types.URI (encodePathSegments)
import           Network.Wreq (delete, get, post)

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

data DepositToAccountRef = DepositToAccountRef
  { depositToAccountRefName  :: !(Maybe String)
  , depositToAccountRefValue :: !String
  }

data SalesTermRef = SalesTermRef
  { salesTermRefValue :: !String
  }

data ShipMethodRef = ShipMethodRef
  { shipMethodRefName  :: !(Maybe String)
  , shipMethodRefValue :: !String
  }

data TxnTaxCodeRef = TxnTaxCodeRef
  { txnTaxCodeRefValue :: !String
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

data EmailAddress = EmailAddress
  { emailAddress :: !String
  }

data TxnTaxDetail = TxnTaxDetail
  { txnTaxDetailTxnTaxCodeRef :: !TxnTaxCodeRef
  , txnTaxDetailTotalTax      :: !Double
  , txnTaxDetailTaxLine       :: !Line
  }

data Invoice = Invoice
  { invoiceId                    :: !(Maybe InvoiceId)
  , invoiceLines                 :: ![Line]
  , invoiceCustomerRef           :: !CustomerRef
  , invoiceDeposit               :: !(Maybe Double)
  , invoiceAllowIPNPayment       :: !(Maybe Bool)
  , invoiceDomain                :: !(Maybe Text)
  , invoiceSparse                :: !(Maybe Bool)
  , invoiceSyncToken             :: !(Maybe String)
  , invoiceMetaData              :: !(Maybe MetaData)

  , invoiceDocNumber             :: !(Maybe String)
  , invoiceTxnDate               :: !(Maybe String)
  , invoiceDepartmentRef         :: !(Maybe DepartmentRef)
  , invoiceCurrencyRef           :: !(Maybe CurrencyRef)
  , invoicePrivateNote           :: !(Maybe String)
  , invoiceTxnTaxDetail          :: !(Maybe TxnTaxDetail)
  , invoiceCustomerMemo          :: !(Maybe String)
  , invoiceBillAddr              :: !(Maybe PhysicalAddress)
  , invoiceShipAddr              :: !(Maybe PhysicalAddress)
  , invoiceClassRef              :: !(Maybe ClassRef)
  , invoiceSalesTermRef          :: !(Maybe SalesTermRef)
  , invoiceDueDate               :: !(Maybe String)
  , invoiceShipMethodRef         :: !(Maybe ShipMethodRef)
  , invoiceShipDate              :: !(Maybe String)
  , invoiceTrackingNum           :: !(Maybe String)
  , invoiceTotalAmt              :: !(Maybe Double)
  , invoiceApplyTaxAfterDiscount :: !(Maybe Bool)
  , invoicePrintStatus           :: !(Maybe String)
  , invoiceEmailStatus           :: !(Maybe String)
  , invoiceBillEmail             :: !(Maybe EmailAddress)
  , invoiceBalance               :: !(Maybe Double)
  , invoiceDepositToAccountRef   :: !(Maybe DepositToAccountRef)
  }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 8}  ''ClassRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 11} ''CurrencyRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 11} ''CustomerRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''DepartmentRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 19} ''DepositToAccountRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''DetailType)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5}  ''EmailAddress)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 7}  ''Invoice)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9}  ''InvoiceId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''Line)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''LineId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 8}  ''MetaData)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 15} ''PhysicalAddress)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''SalesItemLineDetail)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 12} ''SalesTermRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''ShipMethodRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''TxnTaxCodeRef)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 12} ''TxnTaxDetail)

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
