{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}


module QuickBooks.Types where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Aeson.TH
import           Data.Char (toLower)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data APIConfig = APIConfig
  { companyId      :: !ByteString
  , consumerToken  :: !ByteString
  , consumerSecret :: !ByteString
  , oauthToken     :: !ByteString
  , oauthSecret    :: !ByteString
  , hostname       :: !ByteString
  }

data family QuickBooksResponse a
data instance QuickBooksResponse Invoice = QuickBooksInvoiceResponse { quickBooksResponseInvoice :: Invoice }

instance FromJSON (QuickBooksResponse Invoice) where
  parseJSON (Object o) = QuickBooksInvoiceResponse `fmap` (o .: "invoice")
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"

type QuickBooksQuery a = QuickBooksRequest (QuickBooksResponse a)

data QuickBooksRequest a where
--  GetOAuthCredentials :: QuickBooksQuery (ByteString, ByteString)
  CreateInvoice :: Invoice -> QuickBooksQuery Invoice
  ReadInvoice   :: InvoiceId -> QuickBooksQuery Invoice
  UpdateInvoice :: Invoice ->  QuickBooksQuery Invoice
  DeleteInvoice :: InvoiceId  -> SyncToken -> QuickBooksQuery Invoice

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Generic)

newtype LineId    = LineId {unLineId :: Text}
  deriving (Generic)

newtype SyncToken = SyncToken { unSyncToken :: Integer }

data DescriptionLineDetail = DescriptionLineDetail
  { descriptionLineDetailServiceDate :: !(Maybe Text)
  , descriptionLineDetailTaxCodeRef  :: !(Maybe TaxCodeRef)
  }

data DiscountLineDetail = DiscountLineDetail
  { discountLineDetailDiscountRef        :: !(Maybe DiscountRef)
  , discountLineDetailPercentBased       :: !(Maybe Bool)
  , discountLineDetailDiscountPercent    :: !(Maybe Double)
  , discountLineDetailDiscountAccountRef :: !(Maybe DiscountAccountRef)
  }

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

data SubtotalLineDetail = SubtotalLineDetail
  { subtotalLineDetailItemRef :: !(Maybe ItemRef) }

data LineDetailType
  = LineDetailType

data Line = Line
  { lineId                    :: !(Maybe LineId)
  , lineNum                   :: !(Maybe Double)
  , lineDescription           :: !(Maybe Text)
  , lineAmount                :: !Double
  , lineLinkedTxn             :: !(Maybe [LinkedTxn])
  , lineDetailType            :: !LineDetailType
  , lineDescriptionLineDetail :: !(Maybe DescriptionLineDetail)
  , lineDiscountLineDetail    :: !(Maybe DiscountLineDetail)
  , lineSalesItemLineDetail   :: !(Maybe SalesItemLineDetail)
  , lineSubtotalLineDetail    :: !(Maybe SubtotalLineDetail)
  , lineCustomField           :: !(Maybe [CustomField])
  }

data Reference = Reference
  { referenceName  :: Maybe Text
  , referenceType  :: Maybe Text
  , referenceValue :: Text
  }

type ClassRef            = Reference
type CurrencyRef         = Reference
type CustomerRef         = Reference
type DepartmentRef       = Reference
type DepositToAccountRef = Reference
type DiscountAccountRef  = Reference
type DiscountRef         = Reference
type ItemRef             = Reference
type PriceLevelRef       = Reference
type SalesTermRef        = Reference
type ShipMethodRef       = Reference
type TaxCodeRef          = Reference
type TxnTaxCodeRef       = Reference

data ModificationMetaData = ModificationMetaData
  { modificationMetaDataCreateTime      :: !Text
  , modificationMetaDataLastUpdatedTime :: !Text
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

type BillAddr = PhysicalAddress
type ShipAddr = PhysicalAddress

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
  , invoiceSyncToken             :: !(Maybe SyncToken)
  , invoiceMetaData              :: !(Maybe ModificationMetaData)
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
  , invoiceBillAddr              :: !(Maybe BillAddr)
  , invoiceShipAddr              :: !(Maybe ShipAddr)
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

simpleInvoice :: [Line] -> CustomerRef -> Invoice
simpleInvoice simpleInvoiceLine simpleInvoiceCustomerRef =
  Invoice Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          simpleInvoiceLine
          Nothing
          simpleInvoiceCustomerRef
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing

          Nothing
          Nothing
          Nothing

$(deriveJSON defaultOptions
               { fieldLabelModifier = \_ -> "SyncToken" }
              ''SyncToken) 

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 11
               , omitNothingFields  = True }
             ''CustomField)

$(deriveJSON defaultOptions
             ''CustomFieldType)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 12 }
             ''DeliveryInfo)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 21 }
             ''DescriptionLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 18 }
             ''DiscountLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 14 }
             ''LineDetailType)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 5 }
             ''EmailAddress)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 18 }
             ''GlobalTaxModelEnum)

$(deriveFromJSON defaultOptions
               { fieldLabelModifier = drop 7
               , omitNothingFields  = True }
             ''Invoice)

$(deriveToJSON defaultOptions
               { fieldLabelModifier = drop 7
               , omitNothingFields  = True }
             ''Invoice)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 9 }
             ''InvoiceId)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 4 }
             ''Line)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 4 }
             ''LineId)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 9 }
             ''LinkedTxn)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 20 }
             ''ModificationMetaData)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 15 }
             ''PhysicalAddress)

$(deriveJSON defaultOptions
               { fieldLabelModifier = map toLower . drop 9
               , omitNothingFields  = True }
             ''Reference)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 4 }
             ''SalesItemLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 4 }
             ''SubtotalLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 12 }
             ''TxnTaxDetail)
