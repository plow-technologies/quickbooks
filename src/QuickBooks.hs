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
import           Data.Aeson.TH
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

data SalesItemLineDetail = SalesItemLineDetail
  { salesItemLineDetailItemRef         :: !(Maybe ItemRef)
  , salesItemLineDetailClassRef        :: !(Maybe ClassRef)
  , salesItemLineDetailUnitPrice       :: !(Maybe Double)
  , salesItemLineDetailRatePercent     :: !(Maybe Double)
  , salesItemLineDetailPriceLevelRef   :: !(Maybe PriceLevelRef)
  , salesItemLineDetailMarkupInfo      :: !(Maybe Text)
  , salesItemLineDetailQty             :: !(Maybe Double)
  , salesItemLineDetailTaxCodeRef      :: !(Maybe TaxCodeRef)
  , salesItemLineDetailServiceData     :: !(Maybe Text)
  , salesItemLineDetailTaxInclusiveAmt :: !(Maybe Double)
  }

data DetailType = DetailType

data Line = Line
  { lineId                  :: !(Maybe LineId)
  , lineAmount              :: !Double
  , lineDetailType          :: !DetailType
  , lineSalesItemLineDetail :: !SalesItemLineDetail
  }

data Reference = Reference
  { referenceName  :: Maybe Text
  , referenceType  :: Maybe Text
  , referenceValue :: Text
  }

newtype ClassRef = ClassRef { classRef :: Reference }

newtype CurrencyRef = CurrencyRef { currencyRef :: Reference }

newtype CustomerRef = CustomerRef { customerRef :: Reference }

newtype DepartmentRef = DepartmentRef { departmentRef :: Reference }

newtype DepositToAccountRef = DepositToAccountRef { depositToAccountRef :: Reference }

newtype ItemRef = ItemRef { itemRef :: Reference }

newtype PriceLevelRef = PriceLevelRef { priceLevelRef :: Reference }

newtype SalesTermRef = SalesTermRef { salesTermRef :: Reference }

newtype ShipMethodRef = ShipMethodRef { shipMethodRef :: Reference }

newtype TaxCodeRef = TaxCodeRef { taxCodeRef :: Reference }

newtype TxnTaxCodeRef = TxnTaxCodeRef { txnTaxCodeRef :: Reference }

data MetaData = MetaData
  { metaDataCreateTime      :: !Text
  , metaDataLastUpdatedTime :: !Text
  }

data PhysicalAddress = PhysicalAddress
  { physicalAddressId                     :: !Text
  , physicalAddressLine1                  :: !Text
  , physicalAddressLine2                  :: !Text
  , physicalAddressLine3                  :: !Text
  , physicalAddressLine4                  :: !Text
  , physicalAddressLine5                  :: !Text
  , physicalAddressCity                   :: !Text
  , physicalAddressCountry                :: !Text
  , physicalAddressCountrySubDivisionCode :: !Text
  , physicalAddressPostalCode             :: !Text
  , physicalAddressNote                   :: !(Maybe Text)
  , physicalAddressLat                    :: !Text
  , physicalAddressLong                   :: !Text
  }

data EmailAddress = EmailAddress
  { emailAddress :: !Text
  }

data TxnTaxDetail = TxnTaxDetail
  { txnTaxDetailTxnTaxCodeRef :: !TxnTaxCodeRef
  , txnTaxDetailTotalTax      :: !Double
  , txnTaxDetailTaxLine       :: !Line
  }

data DeliveryInfo = DeliveryInfo

data LinkedTxn = LinkedTxn

data CustomField = CustomField
  { customFieldDefinitionId :: !Text
  , customFieldName         :: !Text
  , customFieldType         :: !CustomFieldType
  , customFieldStringValue  :: !(Maybe Text)
  , customFieldBooleanValue :: !(Maybe Bool)
  , customFieldDateValue    :: !(Maybe Text)
  , customFieldNumberValue  :: !(Maybe Double)
  }

data CustomFieldType
  = BooleanType
  | DateType
  | NumberType
  | StringType

data GlobalTaxModelEnum = GlobalTaxModelEnum

data Invoice = Invoice
  { invoiceId                    :: !(Maybe InvoiceId)
  , invoiceSyncToken             :: !(Maybe Text)
  , invoiceMetaData              :: !(Maybe MetaData)
  , invoiceCustomField           :: !(Maybe [CustomField])
  , invoiceDocNumber             :: !(Maybe Text)
  , invoiceTxnDate               :: !(Maybe Text)
  , invoiceDepartmentRef         :: !(Maybe DepartmentRef)
  , invoiceCurrencyRef           :: !(Maybe CurrencyRef) -- Non-US
  , invoiceExchangeRate          :: !(Maybe Double) -- Non-US
  , invoicePrivateNote           :: !(Maybe Text)
  , invoiceLinkedTxn             :: !(Maybe [LinkedTxn])
  , invoiceLine                  :: ![Line]
  , invoiceTxnTaxDetail          :: !(Maybe TxnTaxDetail)
  , invoiceCustomerRef           :: !CustomerRef
  , invoiceCustomerMemo          :: !(Maybe Text)
  , invoiceBillAddr              :: !(Maybe PhysicalAddress)
  , invoiceShipAddr              :: !(Maybe PhysicalAddress)
  , invoiceClassRef              :: !(Maybe ClassRef)
  , invoiceSalesTermRef          :: !(Maybe SalesTermRef)
  , invoiceDueDate               :: !(Maybe Text)
  , invoiceGlobalTaxCalculation  :: !(Maybe GlobalTaxModelEnum) -- Non-US
  , invoiceShipMethodRef         :: !(Maybe ShipMethodRef)
  , invoiceShipDate              :: !(Maybe Text)
  , invoiceTrackingNum           :: !(Maybe Text)
  , invoiceTotalAmt              :: !(Maybe Double)
  , invoiceHomeTotalAmt          :: !(Maybe Double) -- Non-US
  , invoiceApplyTaxAfterDiscount :: !(Maybe Bool)
  , invoicePrintStatus           :: !(Maybe Text)
  , invoiceEmailStatus           :: !(Maybe Text)
  , invoiceBillEmail             :: !(Maybe EmailAddress)
  , invoiceDeliveryInfo          :: !(Maybe DeliveryInfo)
  , invoiceBalance               :: !(Maybe Double)
  , invoiceDepositToAccountRef   :: !(Maybe DepositToAccountRef)
  , invoiceDeposit               :: !(Maybe Double)

  , invoiceAllowIPNPayment       :: !(Maybe Bool)
  , invoiceDomain                :: !(Maybe Text)
  , invoiceSparse                :: !(Maybe Bool)
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
               { fieldLabelModifier = drop 11
               , omitNothingFields  = True }
             ''CustomField)

$(deriveJSON defaultOptions
             ''CustomFieldType)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 12}  ''DeliveryInfo)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "DepartmentRef" }
             ''DepartmentRef)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "DepositToAccountRef" }
             ''DepositToAccountRef)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''DetailType)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5}  ''EmailAddress)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 18}  ''GlobalTaxModelEnum)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 7}  ''Invoice)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9}  ''InvoiceId)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "ItemRef" }
             ''ItemRef)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''Line)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4}  ''LineId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9}  ''LinkedTxn)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 8}  ''MetaData)

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "PriceLevelRef" }
             ''PriceLevelRef)

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
               { fieldLabelModifier = \_ -> "TaxCodeRef" }
             ''TaxCodeRef)

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
