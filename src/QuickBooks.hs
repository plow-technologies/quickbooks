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
import           Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier, omitNothingFields)
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toLower)
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

data Reference = Reference
  { referenceName  :: Maybe String
  , referenceType  :: Maybe String
  , referenceValue :: String
  }

newtype ClassRef = ClassRef { classRef :: Reference }

newtype CurrencyRef = CurrencyRef { currencyRef :: Reference }

newtype DepartmentRef = DepartmentRef { departmentRef :: Reference }

newtype CustomerRef = CustomerRef { customerRef :: Reference }

newtype DepositToAccountRef = DepositToAccountRef { depositToAccountRef :: Reference }

newtype SalesTermRef = SalesTermRef { salesTermRef :: Reference }

newtype ShipMethodRef = ShipMethodRef { shipMethodRef :: Reference }

newtype TxnTaxCodeRef = TxnTaxCodeRef { txnTaxCodeRef :: Reference }

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

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "ClassRef" }
             ''ClassRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "CurrencyRef" }
             ''CurrencyRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "CustomerRef" }
             ''CustomerRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "DepartmentRef" }
             ''DepartmentRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "DepositToAccountRef" }
             ''DepositToAccountRef)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''DetailType)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5}  ''EmailAddress)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 7}  ''Invoice)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9}  ''InvoiceId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''Line)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''LineId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 8}  ''MetaData)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 15} ''PhysicalAddress)

$(deriveJSON defaultOptions
               { fieldLabelModifier = map toLower . drop 9
               , omitNothingFields  = True }
             ''Reference)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''SalesItemLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "SalesTermRef" }
             ''SalesTermRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "ShipMethodRef" }
             ''ShipMethodRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "TxnTaxCodeRef" }
             ''TxnTaxCodeRef)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 12} ''TxnTaxDetail)

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
