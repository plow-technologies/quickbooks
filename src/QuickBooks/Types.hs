{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Types
-- Description :
-- Copyright   :
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
--
--
------------------------------------------------------------------------------

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
  parseJSON (Object o) = QuickBooksInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"

type QuickBooksQuery a = QuickBooksRequest a

data QuickBooksRequest a where
--  GetOAuthCredentials :: QuickBooksQuery (ByteString, ByteString)
  CreateInvoice :: Invoice   -> QuickBooksRequest (QuickBooksResponse Invoice)
  ReadInvoice   :: InvoiceId -> QuickBooksRequest (QuickBooksResponse Invoice)
  UpdateInvoice :: Invoice   -> QuickBooksRequest (QuickBooksResponse Invoice)
  DeleteInvoice :: InvoiceId -> SyncToken -> QuickBooksRequest (QuickBooksResponse Invoice)

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Generic, Show, FromJSON, ToJSON)

newtype LineId    = LineId {unLineId :: Text}
  deriving (Generic, Show, FromJSON, ToJSON)

newtype SyncToken = SyncToken { unSyncToken :: Text }
  deriving (Show, FromJSON, ToJSON)

data DescriptionLineDetail = DescriptionLineDetail
  { descriptionLineDetailServiceDate :: !(Maybe Text)
  , descriptionLineDetailTaxCodeRef  :: !(Maybe TaxCodeRef)
  }
  deriving (Show)

data DiscountLineDetail = DiscountLineDetail
  { discountLineDetailDiscountRef        :: !(Maybe DiscountRef)
  , discountLineDetailPercentBased       :: !(Maybe Bool)
  , discountLineDetailDiscountPercent    :: !(Maybe Double)
  , discountLineDetailDiscountAccountRef :: !(Maybe DiscountAccountRef)
  }
  deriving (Show)

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
  deriving (Show)

data SubTotalLineDetail = SubTotalLineDetail
  { subtotalLineDetailItemRef :: !(Maybe ItemRef) }
  deriving (Show)

data Line = Line
  { lineId                    :: !(Maybe LineId)
  , lineLineNum               :: !(Maybe Double)
  , lineDescription           :: !(Maybe Text)
  , lineAmount                :: !Double
  , lineLinkedTxn             :: !(Maybe [LinkedTxn])
  , lineDetailType            :: !Text
  , lineDescriptionLineDetail :: !(Maybe DescriptionLineDetail)
  , lineDiscountLineDetail    :: !(Maybe DiscountLineDetail)
  , lineSalesItemLineDetail   :: !(Maybe SalesItemLineDetail)
  , lineSubTotalLineDetail    :: !(Maybe SubTotalLineDetail)
  , lineCustomField           :: !(Maybe [CustomField])
  }
  deriving (Show)

data Reference = Reference
  { referenceName  :: !(Maybe Text)
  , referenceType  :: !(Maybe Text)
  , referenceValue :: !Text
  }
  deriving (Show)

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
  deriving (Show)

data PhysicalAddress = PhysicalAddress
  { physicalAddressId                     :: !Text
  , physicalAddressLine1                  :: !Text
  , physicalAddressLine2                  :: !(Maybe Text)
  , physicalAddressLine3                  :: !(Maybe Text)
  , physicalAddressLine4                  :: !(Maybe Text)
  , physicalAddressLine5                  :: !(Maybe Text)
  , physicalAddressCity                   :: !Text
  , physicalAddressCountry                :: !(Maybe Text)
  , physicalAddressCountrySubDivisionCode :: !Text
  , physicalAddressPostalCode             :: !Text
  , physicalAddressNote                   :: !(Maybe Text)
  , physicalAddressLat                    :: !(Maybe Text)
  , physicalAddressLong                   :: !(Maybe Text)
  }
  deriving (Show)

type BillAddr = PhysicalAddress
type ShipAddr = PhysicalAddress

data EmailAddress = EmailAddress
  { emailAddress :: !Text
  }
  deriving (Show)

data TxnTaxDetail = TxnTaxDetail
  { txnTaxDetailTxnTaxCodeRef :: !(Maybe TxnTaxCodeRef)
  , txnTaxDetailTotalTax      :: !Double
  , txnTaxDetailTaxLine       :: !(Maybe Line)
  }
  deriving (Show)

data DeliveryInfo = DeliveryInfo
  { deliveryInfoDeliveryType :: !(Maybe Text)
  , deliveryInfoDeliveryTime :: !(Maybe Text)
  }
  deriving (Show)

data LinkedTxn = LinkedTxn
  { linkedTxnId     :: !(Maybe Text)
  , linkedTxnType   :: !(Maybe Text)
  , linkedTxnLineId :: !(Maybe Text)
  }
  deriving (Show)

data CustomField = CustomField
  { customFieldDefinitionId :: !Text
  , customFieldName         :: !Text
  , customFieldType         :: !CustomFieldType
  , customFieldStringValue  :: !(Maybe Text)
  , customFieldBooleanValue :: !(Maybe Bool)
  , customFieldDateValue    :: !(Maybe Text)
  , customFieldNumberValue  :: !(Maybe Double)
  }
  deriving (Show)

data CustomFieldType
  = BooleanType
  | DateType
  | NumberType
  | StringType
  deriving (Show)

data GlobalTaxModel
  = NotApplicable
  | TaxExcluded
  | TaxInclusive
  deriving (Show)

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
  , invoiceGlobalTaxCalculation  :: !(Maybe GlobalTaxModel) -- Non-US
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
  deriving (Show)

data InvoiceResponse = InvoiceResponse
  { invoiceResponseInvoice :: Invoice }
  deriving (Show)

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
               { fieldLabelModifier = drop 21
               , omitNothingFields  = True }
             ''DescriptionLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 18
               , omitNothingFields  = True }
             ''DiscountLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 5 }
             ''EmailAddress)

$(deriveJSON defaultOptions
             ''GlobalTaxModel)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 7
               , omitNothingFields  = True }
             ''Invoice)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 15 }
             ''InvoiceResponse)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 4 }
             ''Line)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 6
               , omitNothingFields  = True }
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
               { fieldLabelModifier = drop 19
               , omitNothingFields  = True }
             ''SalesItemLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 18
               , omitNothingFields  = True }
             ''SubTotalLineDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 12 }
             ''TxnTaxDetail)
