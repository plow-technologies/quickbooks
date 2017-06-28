{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}

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

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), ToJSON(..), Value (Object),
                                      (.:), object, (.=))
import           Data.Aeson.TH       (Options (fieldLabelModifier, omitNothingFields),
                                      defaultOptions, deriveJSON)
import           Data.ByteString     (ByteString)
import           Data.Char           (toLower)
import           Data.Text           (Text)
import           Data.Text.Encoding  (encodeUtf8, decodeUtf8)
import           Prelude             hiding (lines)
import qualified Text.Email.Validate as E (EmailAddress)
import           System.Log.FastLogger (LoggerSet)
import           Network.HTTP.Client   (Manager)

type Logger = LoggerSet

type CallbackURL = String

newtype OAuthVerifier = OAuthVerifier { unOAuthVerifier :: ByteString }
  deriving (Show, Eq)

-- | QuickBooks Application Keys

data AppConfig = AppConfig
  { consumerToken  :: !ByteString
  , consumerSecret :: !ByteString
  } deriving (Show, Eq)

instance FromJSON AppConfig where
  parseJSON (Object o) = AppConfig <$> (parseByteString o "consumerToken")
                                   <*> (parseByteString o "consumerSecret")
    where parseByteString obj name = encodeUtf8 <$> (obj .: name)
  parseJSON _ = mzero
  
data APIConfig = APIConfig
  { companyId      :: !ByteString
  , oauthToken     :: !ByteString
  , oauthSecret    :: !ByteString
  , hostname       :: !ByteString
  , loggingEnabled :: !ByteString
  } deriving (Show, Eq)

instance FromJSON APIConfig where
  parseJSON (Object o) = APIConfig <$> (parseByteString o "companyId")
                                   <*> (parseByteString o "oauthToken")
                                   <*> (parseByteString o "oauthSecret")
                                   <*> (parseByteString o "hostname")
                                   <*> (parseByteString o "loggingEnabled")
    where parseByteString obj name = encodeUtf8 <$> (obj .: name)
  parseJSON _ = mzero

instance ToJSON APIConfig where
  toJSON (APIConfig cId oToken oSecret hName lEnabled) = object [
                                                                  "companyId" .= (decodeUtf8 cId),
                                                                  "oauthToken" .= (decodeUtf8 oToken),
                                                                  "oauthSecret" .= (decodeUtf8 oSecret),
                                                                  "hostname" .= (decodeUtf8 hName),
                                                                  "loggingEnabled" .= (decodeUtf8 lEnabled)
                                                                ]
                                                                
  
type APIEnv = ( ?apiConfig :: APIConfig
              , AppEnv               
              , NetworkEnv              
              )

type AppEnv = ( ?appConfig :: AppConfig
              , NetworkEnv   
              )
              
type NetworkEnv = ( ?manager :: Manager
                  , ?logger  :: Logger
                  )
             
-- | A request or access OAuth token.

data OAuthToken = OAuthToken
  { token       :: ByteString
  , tokenSecret :: ByteString
  } deriving (Show, Eq)

data family QuickBooksResponse a
data instance QuickBooksResponse Invoice = QuickBooksInvoiceResponse { quickBooksResponseInvoice :: Invoice }
data instance QuickBooksResponse DeletedInvoice = QuickBooksDeletedInvoiceResponse DeletedInvoice
data instance QuickBooksResponse OAuthToken = QuickBooksAuthResponse { tokens :: OAuthToken }
data instance QuickBooksResponse () = QuickBooksVoidResponse

data instance QuickBooksResponse [Customer] =
  QuickBooksCustomerResponse { quickBooksResponseCustomer :: [Customer] }

data instance QuickBooksResponse [Item] =
  QuickBooksItemResponse { quickBooksResponseItem :: [Item] }

instance FromJSON (QuickBooksResponse Invoice) where
  parseJSON (Object o) = QuickBooksInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"


instance FromJSON (QuickBooksResponse DeletedInvoice) where
  parseJSON (Object o) = QuickBooksDeletedInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse deleted invoice response from QuickBooks"

instance FromJSON (QuickBooksResponse [Customer]) where
  parseJSON (Object o) = do
    let customers =
          o .: "QueryResponse" >>= \queryResponse -> queryResponse .: "Customer"
    fmap QuickBooksCustomerResponse customers
  parseJSON _          = fail "Could not parse customer response from QuickBooks"

instance FromJSON (QuickBooksResponse [Item]) where
  parseJSON (Object o) = parseQueryResponse o <|> parseItems o <|> parseSingleItem o
    where
      parseQueryResponse obj = do
        qResponse <- obj .: "QueryResponse"
        parseItems qResponse
      parseItems obj = QuickBooksItemResponse <$> obj .: "Item"
      parseSingleItem obj = do
        i <- obj .: "Item"
        return $ QuickBooksItemResponse [i]
  parseJSON _          = fail "Could not parse item response from QuickBooks"

type QuickBooksQuery a = QuickBooksRequest (QuickBooksResponse a)
type QuickBooksOAuthQuery a = QuickBooksOAuthRequest (QuickBooksResponse a) 

data QuickBooksOAuthRequest a where
  GetTempOAuthCredentials :: CallbackURL   -> QuickBooksOAuthQuery OAuthToken
  GetAccessTokens         :: OAuthVerifier -> QuickBooksOAuthQuery OAuthToken
  DisconnectQuickBooks    :: QuickBooksOAuthQuery ()

data QuickBooksRequest a where
  CreateInvoice           :: Invoice     -> QuickBooksQuery Invoice
  ReadInvoice             :: InvoiceId   -> QuickBooksQuery Invoice
  UpdateInvoice           :: Invoice     -> QuickBooksQuery Invoice
  DeleteInvoice           :: InvoiceId   -> SyncToken -> QuickBooksQuery DeletedInvoice
  SendInvoice             :: InvoiceId   -> E.EmailAddress -> QuickBooksQuery Invoice

  QueryCustomer           :: Text -> QuickBooksQuery [Customer]
  CreateItem              :: Item -> QuickBooksQuery [Item]
  ReadItem                :: Text -> QuickBooksQuery [Item]
  UpdateItem              :: Item -> QuickBooksQuery [Item]
  DeleteItem              :: Item -> QuickBooksQuery [Item]
  QueryItem               :: Text -> QuickBooksQuery [Item]

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
-- In order to create a sales item line detail, use 'salesItemLineDetail'.

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

-- | Create a sales item line detail with a reference to an item.
--
-- Example:
--
-- >>> let aSalesItemLineDetail = salesItemLineDetail ((reference "1") {referenceName = Just "Services"})
-- >>> salesItemLineDetailItemRef aSalesItemLineDetail
-- Just (Reference {referenceName = Just "Services", referenceType = Nothing, referenceValue = "1"})

salesItemLineDetail :: ItemRef -> SalesItemLineDetail
salesItemLineDetail itemRef =
  SalesItemLineDetail (Just itemRef)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing

-- | Details of a subtotal line.

data SubTotalLineDetail = SubTotalLineDetail
  { subtotalLineDetailItemRef :: !(Maybe ItemRef) }
  deriving (Show, Eq)

-- | An individual line item of a transaction.

data Line = Line
  { lineId                    :: !(Maybe LineId)
  , lineLineNum               :: !(Maybe Double)
  , lineDescription           :: !(Maybe Text)
  , lineAmount                :: !(Maybe Double)
  , lineLinkedTxn             :: !(Maybe [LinkedTxn])
  , lineDetailType            :: !Text
  , lineDescriptionLineDetail :: !(Maybe DescriptionLineDetail)
  , lineDiscountLineDetail    :: !(Maybe DiscountLineDetail)
  , lineSalesItemLineDetail   :: !(Maybe SalesItemLineDetail)
  , lineSubTotalLineDetail    :: !(Maybe SubTotalLineDetail)
  , lineCustomField           :: !(Maybe [CustomField])
  }
  deriving (Show, Eq)

-- | Create a sales item line with amount and details.
--
-- Example:
--
-- >>> let aSalesItemLineDetail = salesItemLineDetail ((reference "1") {referenceName = Just "Services"})
-- >>> let aSalesItemLine = salesItemLine 100.0 aSalesItemLineDetail

salesItemLine :: Double
              -> SalesItemLineDetail
              -> Line
salesItemLine amount detail =
  Line Nothing
       Nothing
       Nothing
       (Just amount)
       Nothing
       "SalesItemLineDetail"
       Nothing
       Nothing
       (Just detail)
       Nothing
       Nothing
-- | if you are using a salesItemLineDetail that has a Qty and a price it generates amount automatically
emptySalesItemLine :: Line
emptySalesItemLine =
  Line Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       "SalesItemLineDetail"
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing


newtype DeletedInvoiceId = DeletedInvoiceId { unDeletedInvoiceId :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

data DeletedInvoice = DeletedInvoice
  { deletedInvoiceId     :: !DeletedInvoiceId
  , deletedInvoicedomain :: !Text
  , deletedInvoicestatus :: !Text
  } deriving (Show, Eq)

-- | A reference.
-- In order to create a reference, use 'reference'.

data Reference = Reference
  { referenceName  :: !(Maybe Text)
  , referenceType  :: !(Maybe Text)
  , referenceValue :: !Text
  }
  deriving (Show, Eq)

-- | Create a reference with a value.
--
-- Example:
--
-- >>> reference "21"
-- Reference {referenceName = Nothing, referenceType = Nothing, referenceValue = "21"}

reference :: Text -> Reference
reference = Reference Nothing Nothing

type ClassRef            = Reference
type CurrencyRef         = Reference

-- | A reference to a customer or a job.
--
-- Example:
--
-- >>> (reference "21") {referenceName = Just "John Doe"} :: CustomerRef
-- Reference {referenceName = Just "John Doe", referenceType = Nothing, referenceValue = "21"}

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

data TelephoneNumber = TelephoneNumber
  { telephoneNumberFreeFormNumber :: !Text
  }
  deriving (Eq, Show)

data WebSiteAddress = WebAddress
  { webSiteAddressURI :: !Text
  }
  deriving (Eq, Show)

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
-- In order to create an invoice, use 'defaultInvoice'.

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

-- | Create an 'Invoice' with the minimum elements.
--
-- Example:
--
-- >>> let customer21 = reference "21" :: CustomerRef
-- >>> let aSalesItemLineDetail = salesItemLineDetail ((reference "1") {referenceName = Just "Services"})
-- >>> let aSalesItemLine = salesItemLine 100.0 aSalesItemLineDetail
--
-- >>> let anInvoice = defaultInvoice [aSalesItemLine] customer21

defaultInvoice :: [Line]      -- ^ The line items of a transaction
               -> CustomerRef -- ^ Reference to a customer or a job
               -> Invoice
defaultInvoice [] _ = error "Bad invoice"
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

data CustomerResponse = CustomerResponse
  { customerResponseCustomer :: !Customer
  }
  deriving (Eq, Show)

data Customer = Customer
  { customerId                      :: !(Maybe Text)
  , customerSyncToken               :: !(Maybe SyncToken)
  , customerMetaData                :: !(Maybe ModificationMetaData)
  , customerTitle                   :: !(Maybe Text) -- def null
  , customerGivenName               :: !(Maybe Text) -- max 25 def null
  , customerMiddleName              :: !(Maybe Text) -- max 25, def null
  , customerFamilyName              :: !(Maybe Text) -- max 25, def null
  , customerSuffix                  :: !(Maybe Text) -- max 10, def null
  , customerFullyQualifiedName      :: !(Maybe Text)
  , customerCompanyName             :: !(Maybe Text) -- max 50, def null
  , customerDisplayName             :: !Text -- unique
  , customerPrintOnCheckName        :: !(Maybe Text) -- max 100
  , customerActive                  :: !(Maybe Bool) -- def true
  , customerPrimaryPhone            :: !(Maybe TelephoneNumber)
  , customerAlternatePhone          :: !(Maybe TelephoneNumber)
  , customerMobile                  :: !(Maybe TelephoneNumber)
  , customerFax                     :: !(Maybe TelephoneNumber)
  , customerPrimaryEmailAddress     :: !(Maybe EmailAddress)
  , customerWebAddr                 :: !(Maybe WebSiteAddress)
  , customerDefaultTaxCodeRef       :: !(Maybe TaxCodeRef)
  , customerTaxable                 :: !(Maybe Bool)
  , customerBillAddr                :: !(Maybe BillAddr)
  , customerShipAddr                :: !(Maybe ShipAddr)
  , customerNotes                   :: !(Maybe Text) -- max 2000
  , customerJob                     :: !(Maybe Bool) -- def false or null
  , customerBillWithParent          :: !(Maybe Bool) -- def false or null
  , customerParentRef               :: !(Maybe CustomerRef)
  , customerLevel                   :: !(Maybe Int) -- def 0, up to 5
  , customerSalesTermRef            :: !(Maybe SalesTermRef)
  , customerPaymentMethodRef        :: !(Maybe Reference)
  , customerBalance                 :: !(Maybe Double)
  , customerOpenBalanceDate         :: !(Maybe Text)
  , customerBalanceWithJobs         :: !(Maybe Double)
  , customerCurrencyRef             :: !(Maybe CurrencyRef)
  , customerPreferredDeliveryMethod :: !(Maybe Text)
  , customerResaleNum               :: !(Maybe Text) -- max 15
  }
  deriving (Eq, Show)

data ItemResponse = ItemResponse
  { itemResponseItem :: !Item
  }
  deriving (Eq, Show)

data Item = Item
  { itemId                   :: !(Maybe Text)
  , itemSyncToken            :: !(Maybe SyncToken)
  , itemMetaData             :: !(Maybe ModificationMetaData)
  , itemName                 :: !Text -- max 100
  , itemDescription          :: !(Maybe Text) -- max 4000
  , itemActive               :: !(Maybe Bool) -- def true
  , itemSubItem              :: !(Maybe Bool) -- def false or null
  , itemParentRef            :: !(Maybe ItemRef) -- def null
  , itemLevel                :: !(Maybe Int) -- def 0 up to 5
  , itemFullyQualifiedName   :: !(Maybe String) -- def null
  , itemTaxable              :: !(Maybe Bool) -- US only
  , itemSalesTaxInclusive    :: !(Maybe Bool) -- def false
  , itemUnitPrice            :: !(Maybe Double) -- max 99999999999, def 0
  , itemType                 :: !(Maybe Text) -- def Inventory (Inventory/Service)
  , itemIncomeAccountRef     :: !(Maybe Reference) -- required
  , itemPurchaseDesc         :: !(Maybe String) -- max 1000
  , itemPurchaseTaxInclusive :: !(Maybe Bool) -- def false
  , itemPurchaseCost         :: !(Maybe Double) -- max 99999999999
  , itemExpenseAccountRef    :: !(Maybe Reference) -- required
  , itemAssetAccountRef      :: !(Maybe Reference) -- req for inventory items
  , itemTrackQtyOnHand       :: !(Maybe Bool) -- def false
  , itemQtyOnHand            :: !(Maybe Double) -- req for inventory items
  , itemSalesTaxCodeRef      :: !(Maybe Reference)
  , itemPurchaseTaxCodeRef   :: !(Maybe Reference)
  , itemInvStartDate         :: !(Maybe Text) -- required for inventory items
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 8
               , omitNothingFields  = True
               }
             ''Customer)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 16
               }
             ''CustomerResponse)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 4
               , omitNothingFields  = True
               }
             ''Item)


$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 12
               }
             ''ItemResponse)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 15
               }
             ''TelephoneNumber)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 14
               }
             ''WebSiteAddress)

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
