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

import Data.Aeson      ((.:),FromJSON(..),Value(Object),ToJSON)
import Data.ByteString (ByteString)
import Data.Aeson.TH   (defaultOptions
                       ,deriveJSON
                       ,Options(fieldLabelModifier,omitNothingFields))
import Data.Char       (toLower)
import Data.Text       (Text)
import qualified Text.Email.Validate as E (EmailAddress)
import Prelude hiding (lines)

type CallbackURL = String

newtype OAuthVerifier = OAuthVerifier { unOAuthVerifier :: ByteString }
  deriving (Show, Eq)

data APIConfig = APIConfig
  { companyId      :: !ByteString
  , consumerToken  :: !ByteString
  , consumerSecret :: !ByteString
  , oauthToken     :: !ByteString
  , oauthSecret    :: !ByteString
  , hostname       :: !ByteString
  , loggingEnabled :: !ByteString 
  } deriving (Show, Eq)

-- | A request or access OAuth token.

data OAuthToken = OAuthToken
  { token         :: ByteString
  , tokenSecret   :: ByteString
  } deriving (Show, Eq)

data family QuickBooksResponse a
data instance QuickBooksResponse Invoice = QuickBooksInvoiceResponse { quickBooksResponseInvoice :: Invoice }
data instance QuickBooksResponse DeletedInvoice = QuickBooksDeletedInvoiceResponse DeletedInvoice
data instance QuickBooksResponse OAuthToken = QuickBooksAuthResponse { tokens :: OAuthToken }
data instance QuickBooksResponse () = QuickBooksVoidResponse 

instance FromJSON (QuickBooksResponse Invoice) where
  parseJSON (Object o) = QuickBooksInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"

instance FromJSON (QuickBooksResponse DeletedInvoice) where
  parseJSON (Object o) = QuickBooksDeletedInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse deleted invoice response from QuickBooks"

type QuickBooksQuery a = QuickBooksRequest (QuickBooksResponse a)

data QuickBooksRequest a where
  GetTempOAuthCredentials :: CallbackURL -> QuickBooksQuery OAuthToken
  GetAccessTokens         :: OAuthVerifier -> QuickBooksQuery OAuthToken
  CreateInvoice           :: Invoice     -> QuickBooksQuery Invoice
  ReadInvoice             :: InvoiceId   -> QuickBooksQuery Invoice
  UpdateInvoice           :: Invoice     -> QuickBooksQuery Invoice
  DeleteInvoice           :: InvoiceId   -> SyncToken -> QuickBooksQuery DeletedInvoice
  SendInvoice             :: InvoiceId   -> E.EmailAddress -> QuickBooksQuery Invoice
  DisconnectQuickBooks    :: QuickBooksQuery ()

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Show, Eq, FromJSON, ToJSON)

newtype LineId    = LineId {unLineId :: Text}
  deriving (Show, Eq, FromJSON, ToJSON)

newtype SyncToken = SyncToken { unSyncToken :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Details of a description line.

data DescriptionLineDetail = DescriptionLineDetail
  { descriptionLineDetailServiceDate :: !(Maybe Text)
  , descriptionLineDetailTaxCodeRef  :: !(Maybe TaxCodeRef)
  }
  deriving (Show, Eq)

-- | Details of a discount line.

data DiscountLineDetail = DiscountLineDetail
  { discountLineDetailDiscountRef        :: !(Maybe DiscountRef)
  , discountLineDetailPercentBased       :: !(Maybe Bool)
  , discountLineDetailDiscountPercent    :: !(Maybe Double)
  , discountLineDetailDiscountAccountRef :: !(Maybe DiscountAccountRef)
  }
  deriving (Show, Eq)

-- | Details of a sales item line.

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
  deriving (Show, Eq)

-- | Details of a subtotal line.

data SubTotalLineDetail = SubTotalLineDetail
  { subtotalLineDetailItemRef :: !(Maybe ItemRef) }
  deriving (Show, Eq)

-- | An individual line item of a transaction.

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
  deriving (Show, Eq)

newtype DeletedInvoiceId = DeletedInvoiceId { unDeletedInvoiceId :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

data DeletedInvoice = DeletedInvoice 
  { deletedInvoiceId     :: !DeletedInvoiceId
  , deletedInvoicedomain :: !Text
  , deletedInvoicestatus :: !Text
  } deriving (Show, Eq)

data Reference = Reference
  { referenceName  :: !(Maybe Text)
  , referenceType  :: !(Maybe Text)
  , referenceValue :: !Text
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

type BillAddr = PhysicalAddress
type ShipAddr = PhysicalAddress

data EmailAddress = EmailAddress
  { emailAddress :: !Text
  }
  deriving (Show, Eq)

data TxnTaxDetail = TxnTaxDetail
  { txnTaxDetailTxnTaxCodeRef :: !(Maybe TxnTaxCodeRef)
  , txnTaxDetailTotalTax      :: !Double
  , txnTaxDetailTaxLine       :: !(Maybe Line)
  }
  deriving (Show, Eq)

data DeliveryInfo = DeliveryInfo
  { deliveryInfoDeliveryType :: !(Maybe Text)
  , deliveryInfoDeliveryTime :: !(Maybe Text)
  }
  deriving (Show, Eq)

data LinkedTxn = LinkedTxn
  { linkedTxnId     :: !(Maybe Text)
  , linkedTxnType   :: !(Maybe Text)
  , linkedTxnLineId :: !(Maybe Text)
  }
  deriving (Show, Eq)

data CustomField = CustomField
  { customFieldDefinitionId :: !Text
  , customFieldName         :: !Text
  , customFieldType         :: !CustomFieldType
  , customFieldStringValue  :: !(Maybe Text)
  , customFieldBooleanValue :: !(Maybe Bool)
  , customFieldDateValue    :: !(Maybe Text)
  , customFieldNumberValue  :: !(Maybe Double)
  }
  deriving (Show, Eq)

data CustomFieldType
  = BooleanType
  | DateType
  | NumberType
  | StringType
  deriving (Show, Eq)

data GlobalTaxModel
  = NotApplicable
  | TaxExcluded
  | TaxInclusive
  deriving (Show, Eq)

-- | An invoice transaction entity, that is, a sales form where the customer
-- pays for a product or service later.
--
-- Business rules:
--
--   * An invoice must have at least one 'Line' that describes an item.
--   * An invoice must have a 'CustomerRef'.
--
-- In order to create an invoice, use the 'defaultInvoice' function.

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
  deriving (Show, Eq)

defaultInvoice :: [Line] -> CustomerRef -> Invoice
defaultInvoice lines customerRef =
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
          lines
          Nothing
          customerRef
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

data InvoiceResponse = InvoiceResponse
  { invoiceResponseInvoice :: Invoice }
  deriving (Show, Eq)

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

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop (length ("deletedInvoice" :: String)) }
             ''DeletedInvoice)
