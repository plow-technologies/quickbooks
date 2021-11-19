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
import           QuickBooks.QBText
import qualified Network.OAuth.OAuth2            as OAuth2
type Logger = LoggerSet

type CallbackURL = String

newtype OAuthVerifier = OAuthVerifier { unOAuthVerifier :: ByteString }
  deriving (Show, Eq, Ord)

-- | QuickBooks Application Keys

data AppConfig = AppConfig
  { consumerToken  :: !ByteString
  , consumerSecret :: !ByteString
  } deriving (Show, Eq, Ord)

instance FromJSON AppConfig where
  parseJSON (Object o) = AppConfig <$> (parseByteString o "consumerToken")
                                   <*> (parseByteString o "consumerSecret")
    where parseByteString obj name = encodeUtf8 <$> (obj .: name)
  parseJSON _ = mzero


-- Config types for OAuth2 data
data OAuth2Config = OAuth2Config
  { oauthClientId      :: !Text
  , oauthClientSecret  :: !Text
  , oauthRefreshToken  :: !Text
  } deriving (Show)

instance FromJSON OAuth2Config where
  parseJSON (Object o) = OAuth2Config <$> o .: "oauth2ClientId"
                                      <*> o .: "oauth2ClientSecret"
                                      <*> o .: "oauth2RefreshToken"

  parseJSON _ = mzero


data APIConfig = APIConfig
  { companyId          :: !ByteString
  , hostname           :: !ByteString
  , loggingEnabled     :: !ByteString
  } deriving (Show, Eq, Ord)



instance ToJSON APIConfig where
  toJSON (APIConfig cId hName lEnabled) = object [
                                                                  "companyId" .= (decodeUtf8 cId),
                                                                  "hostname" .= (decodeUtf8 hName),
                                                                  "loggingEnabled" .= (decodeUtf8 lEnabled)
                                                                ]

instance FromJSON APIConfig where
  parseJSON (Object o) = APIConfig <$> (parseByteString o "companyId")
                                   <*> (parseByteString o "hostname")
                                   <*> (parseByteString o "loggingEnabled")
    where parseByteString  obj name = encodeUtf8 <$> (obj .: name)
  parseJSON _ = mzero



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


data OAuthTokens = OAuth1 OAuthToken
                 | OAuth2 OAuth2.AccessToken


data OAuthToken = OAuthToken
  { token       :: ByteString
  , tokenSecret :: ByteString
  } deriving (Show, Eq, Ord)

data family QuickBooksResponse a
data instance QuickBooksResponse Invoice = QuickBooksInvoiceResponse { quickBooksResponseInvoice :: Invoice }
data instance QuickBooksResponse DeletedInvoice = QuickBooksDeletedInvoiceResponse DeletedInvoice
data instance QuickBooksResponse DeletedCategory = QuickBooksDeletedCategoryResponse DeletedCategory
data instance QuickBooksResponse OAuthToken = QuickBooksAuthResponse { tokens :: OAuthToken }
data instance QuickBooksResponse () = QuickBooksVoidResponse

data instance QuickBooksResponse [Customer] =
  QuickBooksCustomerResponse { quickBooksResponseCustomer :: [Customer] }

data instance QuickBooksResponse [Item] =
  QuickBooksItemResponse { quickBooksResponseItem :: [Item] }
  deriving (Show)

data instance QuickBooksResponse [Bundle] =
  QuickBooksBundleResponse { quickBooksResponseBundle :: [Bundle] }

data instance QuickBooksResponse [Category] =
  QuickBooksCategoryResponse { quickBooksResponseCategory :: [Category] }

data instance QuickBooksResponse Int =
  QuickBooksCountResponse { quickBooksCountResponse :: Int}

instance FromJSON (QuickBooksResponse Int) where
  parseJSON (Object o) = parseQueryResponse o
    where
      parseQueryResponse obj = do
        qResponse <- obj .: "QueryResponse"
        parseInt qResponse <|> (return $ QuickBooksCountResponse (-1))
      parseInt obj = QuickBooksCountResponse <$> obj .: "totalCount"
  parseJSON _          = fail "Could not parse count response from QuickBooks"

instance FromJSON (QuickBooksResponse Invoice) where
  parseJSON (Object o) = QuickBooksInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse invoice response from QuickBooks"


instance FromJSON (QuickBooksResponse DeletedInvoice) where
  parseJSON (Object o) = QuickBooksDeletedInvoiceResponse `fmap` (o .: "Invoice")
  parseJSON _          = fail "Could not parse deleted invoice response from QuickBooks"

instance FromJSON (QuickBooksResponse [Customer]) where
  parseJSON (Object o) = parseQueryResponse o <|> parseCustomers o <|> parseSingleCustomer o
    where
      parseQueryResponse obj = do
        qResponse <- obj .: "QueryResponse"
        parseCustomers qResponse <|> (return (QuickBooksCustomerResponse []))
      parseCustomers obj = QuickBooksCustomerResponse <$> obj .: "Customer"
      parseSingleCustomer obj = do
        i <- obj .: "Customer"
        return $ QuickBooksCustomerResponse [i]
  parseJSON _          = fail "Could not parse customer response from QuickBooks"

instance FromJSON (QuickBooksResponse [Item]) where
  parseJSON (Object o) = parseQueryResponse o <|> parseItems o <|> parseSingleItem o
    where
      parseQueryResponse obj = do
        qResponse <- obj .: "QueryResponse"
        parseItems qResponse <|> (return (QuickBooksItemResponse []))
      parseItems obj = QuickBooksItemResponse <$> obj .: "Item"
      parseSingleItem obj = do
        i <- obj .: "Item"
        return $ QuickBooksItemResponse [i]
  parseJSON _          = fail "Could not parse item response from QuickBooks"

-- Bundles still have an Item response from the QB API
instance FromJSON (QuickBooksResponse [Bundle]) where
  parseJSON (Object o) = parseQueryResponse o <|> parseBundles o <|> parseSingleBundle o
    where
      parseQueryResponse obj = do
        qResponse <- obj .: "QueryResponse"
        parseBundles qResponse <|> (return (QuickBooksBundleResponse []))
      parseBundles obj = QuickBooksBundleResponse <$> obj .: "Item"
      parseSingleBundle obj = do
        i <- obj .: "Item"
        return $ QuickBooksBundleResponse [i]
  parseJSON _          = fail "Could not parse bundle response from QuickBooks"

-- Categories still have an Item response from the QB API
instance FromJSON (QuickBooksResponse [Category]) where
  parseJSON (Object o) = parseQueryResponse o <|> parseCategories o <|> parseSingleCategory o
    where
      parseQueryResponse obj = do
        qResponse <- obj .: "QueryResponse"
        parseCategories qResponse <|> (return (QuickBooksCategoryResponse []))
      parseCategories obj = QuickBooksCategoryResponse <$> obj .: "Item"
      parseSingleCategory obj = do
        i <- obj .: "Item"
        return $ QuickBooksCategoryResponse [i]
  parseJSON _          = fail "Could not parse category response from QuickBooks"

instance FromJSON (QuickBooksResponse DeletedCategory) where
  parseJSON (Object o) = QuickBooksDeletedCategoryResponse `fmap` (o .: "Item")
  parseJSON _          = fail "Could not parse deleted category response from QuickBooks"


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

  CreateCustomer          :: Customer -> QuickBooksQuery [Customer]
  ReadCustomer            :: Text -> QuickBooksQuery [Customer]
  UpdateCustomer          :: Customer -> QuickBooksQuery [Customer]
  DeleteCustomer          :: Customer -> QuickBooksQuery [Customer]
  QueryCustomer           :: Text -> QuickBooksQuery [Customer]
  QueryCountCustomer      :: (QuickBooksQuery Int)
  QueryMaxCustomersFrom   :: Int -> QuickBooksQuery [Customer]

  CreateItem              :: Item -> QuickBooksQuery [Item]
  ReadItem                :: Text -> QuickBooksQuery [Item]
  UpdateItem              :: Item -> QuickBooksQuery [Item]
  DeleteItem              :: Item -> QuickBooksQuery [Item]
  QueryItem               :: Text -> QuickBooksQuery [Item]
  QueryCountItem          :: (QuickBooksQuery Int)
  QueryMaxItemsFrom       :: Int -> QuickBooksQuery [Item]

  ReadBundle              :: Text -> QuickBooksQuery [Bundle]
  QueryBundle             :: Text -> QuickBooksQuery [Bundle]

  CreateCategory          :: Category -> QuickBooksQuery [Category]
  ReadCategory            :: Text -> QuickBooksQuery [Category]
  UpdateCategory          :: Category -> QuickBooksQuery [Category]
  DeleteCategory          :: Category -> QuickBooksQuery DeletedCategory
  QueryCategory           :: Text -> QuickBooksQuery [Category]
  QueryCountCategory      :: (QuickBooksQuery Int)
  QueryMaxCategoriesFrom  :: Int -> QuickBooksQuery [Category]

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype LineId    = LineId {unLineId :: Text}
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype SyncToken = SyncToken { unSyncToken :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

-- | Details of a description line.

data DescriptionLineDetail = DescriptionLineDetail
  { descriptionLineDetailServiceDate :: !(Maybe Text)
  , descriptionLineDetailTaxCodeRef  :: !(Maybe TaxCodeRef)
  }
  deriving (Show, Eq, Ord)

-- | Details of a discount line.

data DiscountLineDetail = DiscountLineDetail
  { discountLineDetailDiscountRef        :: !(Maybe DiscountRef)
  , discountLineDetailPercentBased       :: !(Maybe Bool)
  , discountLineDetailDiscountPercent    :: !(Maybe Double)
  , discountLineDetailDiscountAccountRef :: !(Maybe DiscountAccountRef)
  }
  deriving (Show, Eq, Ord)

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
  , salesItemLineDetailServiceDate     :: !(Maybe Text)
  , salesItemLineDetailTaxInclusiveAmt :: !(Maybe Double)
  }
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

-- | An individual line item of a transaction.

data ItemGroupDetail = ItemGroupDetail
  { itemGroupLine             :: !(Maybe [ItemGroupLine])}
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data DeletedInvoice = DeletedInvoice
  { deletedInvoiceId     :: !DeletedInvoiceId
  , deletedInvoicedomain :: !Text
  , deletedInvoicestatus :: !Text
  } deriving (Show, Eq, Ord)

-- | A reference.
-- In order to create a reference, use 'reference'.

data ItemGroupLine = ItemGroupLine
  { itemRef        :: !(Maybe ItemRef)
  , itemQty        :: !(Maybe Integer)
  }
  deriving (Show, Eq, Ord)

data Reference = Reference
  { referenceName  :: !(Maybe Text)
  , referenceType  :: !(Maybe Text)
  , referenceValue :: !Text
  }
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

data TelephoneNumber = TelephoneNumber
  { telephoneNumberFreeFormNumber :: !Text
  }
  deriving (Eq, Ord, Show)

data WebSiteAddress = WebAddress
  { webSiteAddressURI :: !Text
  }
  deriving (Eq, Ord, Show)

data PhysicalAddress = PhysicalAddress
  { physicalAddressId                     :: !(Maybe Text)
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
  deriving (Show, Eq, Ord)

type BillAddr = PhysicalAddress
type ShipAddr = PhysicalAddress

data EmailAddress = EmailAddress
  { emailAddress :: !Text
  }
  deriving (Show, Eq, Ord)

data TxnTaxDetail = TxnTaxDetail
  { txnTaxDetailTxnTaxCodeRef :: !(Maybe TxnTaxCodeRef)
  , txnTaxDetailTotalTax      :: !Double
  , txnTaxDetailTaxLine       :: !(Maybe Line)
  }
  deriving (Show, Eq, Ord)

data DeliveryInfo = DeliveryInfo
  { deliveryInfoDeliveryType :: !(Maybe Text)
  , deliveryInfoDeliveryTime :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord)

data LinkedTxn = LinkedTxn
  { linkedTxnId     :: !(Maybe Text)
  , linkedTxnType   :: !(Maybe Text)
  , linkedTxnLineId :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord)

data CustomField = CustomField
  { customFieldDefinitionId :: !Text
  , customFieldName         :: !Text
  , customFieldType         :: !CustomFieldType
  , customFieldStringValue  :: !(Maybe Text)
  , customFieldBooleanValue :: !(Maybe Bool)
  , customFieldDateValue    :: !(Maybe Text)
  , customFieldNumberValue  :: !(Maybe Double)
  }
  deriving (Show, Eq, Ord)

data CustomFieldType
  = BooleanType
  | DateType
  | NumberType
  | StringType
  deriving (Show, Eq, Ord)

data GlobalTaxModel
  = NotApplicable
  | TaxExcluded
  | TaxInclusive
  deriving (Show, Eq, Ord)

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
  , invoiceDocNumber             :: !(Maybe QBText)
  , invoiceTxnDate               :: !(Maybe QBText)
  , invoiceDepartmentRef         :: !(Maybe DepartmentRef)
  , invoiceCurrencyRef           :: !(Maybe CurrencyRef) -- Non-US
  , invoiceExchangeRate          :: !(Maybe Double) -- Non-US
  , invoicePrivateNote           :: !(Maybe QBText)
  , invoiceLinkedTxn             :: !(Maybe [LinkedTxn])
  , invoiceLine                  :: ![Line]
  , invoiceTxnTaxDetail          :: !(Maybe TxnTaxDetail)
  , invoiceCustomerRef           :: !CustomerRef
  , invoiceCustomerMemo          :: !(Maybe QBText)
  , invoiceBillAddr              :: !(Maybe BillAddr)
  , invoiceShipAddr              :: !(Maybe ShipAddr)
  , invoiceClassRef              :: !(Maybe ClassRef)
  , invoiceSalesTermRef          :: !(Maybe SalesTermRef)
  , invoiceDueDate               :: !(Maybe QBText)
  , invoiceGlobalTaxCalculation  :: !(Maybe GlobalTaxModel) -- Non-US
  , invoiceShipMethodRef         :: !(Maybe ShipMethodRef)
  , invoiceShipDate              :: !(Maybe QBText)
  , invoiceTrackingNum           :: !(Maybe QBText)
  , invoiceTotalAmt              :: !(Maybe Double)
  , invoiceHomeTotalAmt          :: !(Maybe Double) -- Non-US
  , invoiceApplyTaxAfterDiscount :: !(Maybe Bool)
  , invoicePrintStatus           :: !(Maybe QBText)
  , invoiceEmailStatus           :: !(Maybe QBText)
  , invoiceBillEmail             :: !(Maybe EmailAddress)
  , invoiceDeliveryInfo          :: !(Maybe DeliveryInfo)
  , invoiceBalance               :: !(Maybe Double)
  , invoiceDepositToAccountRef   :: !(Maybe DepositToAccountRef)
  , invoiceDeposit               :: !(Maybe Double)

  , invoiceAllowIPNPayment       :: !(Maybe Bool)
  , invoiceDomain                :: !(Maybe QBText)
  , invoiceSparse                :: !(Maybe Bool)
  }
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

data CustomerResponse = CustomerResponse
  { customerResponseCustomer :: !Customer
  }
  deriving (Eq, Ord, Show)

data Customer = Customer
  { customerId                      :: !(Maybe QBText)
  , customerSyncToken               :: !(Maybe SyncToken)
  , customerMetaData                :: !(Maybe ModificationMetaData)
  , customerTitle                   :: !(Maybe QBText) -- def null
  , customerGivenName               :: !(Maybe QBText) -- max 25 def null
  , customerMiddleName              :: !(Maybe QBText) -- max 25, def null
  , customerFamilyName              :: !(Maybe QBText) -- max 25, def null
  , customerSuffix                  :: !(Maybe QBText) -- max 10, def null
  , customerFullyQualifiedName      :: !(Maybe QBText)
  , customerCompanyName             :: !(Maybe QBText) -- max 50, def null
  , customerDisplayName             :: !QBText -- unique
  , customerPrintOnCheckName        :: !(Maybe QBText) -- max 100
  , customerActive                  :: !(Maybe Bool) -- def true
  , customerPrimaryPhone            :: !(Maybe TelephoneNumber)
  , customerAlternatePhone          :: !(Maybe TelephoneNumber)
  , customerMobile                  :: !(Maybe TelephoneNumber)
  , customerFax                     :: !(Maybe TelephoneNumber)
  , customerPrimaryEmailAddr        :: !(Maybe EmailAddress)
  , customerWebAddr                 :: !(Maybe WebSiteAddress)
  , customerDefaultTaxCodeRef       :: !(Maybe TaxCodeRef)
  , customerTaxable                 :: !(Maybe Bool)
  , customerBillAddr                :: !(Maybe BillAddr)
  , customerShipAddr                :: !(Maybe ShipAddr)
  , customerNotes                   :: !(Maybe QBText) -- max 2000
  , customerJob                     :: !(Maybe Bool) -- def false or null
  , customerBillWithParent          :: !(Maybe Bool) -- def false or null
  , customerParentRef               :: !(Maybe CustomerRef)
  , customerLevel                   :: !(Maybe Int) -- def 0, up to 5
  , customerSalesTermRef            :: !(Maybe SalesTermRef)
  , customerPaymentMethodRef        :: !(Maybe Reference)
  , customerBalance                 :: !(Maybe Double)
  , customerOpenBalanceDate         :: !(Maybe QBText)
  , customerBalanceWithJobs         :: !(Maybe Double)
  , customerCurrencyRef             :: !(Maybe CurrencyRef)
  , customerPreferredDeliveryMethod :: !(Maybe QBText)
  , customerResaleNum               :: !(Maybe QBText) -- max 15
  }
  deriving (Eq, Ord, Show)

data ItemResponse = ItemResponse
  { itemResponseItem :: !Item
  }
  deriving (Eq, Ord, Show)

data Item = Item
  { itemId                   :: !(Maybe QBText)
  , itemSyncToken            :: !(Maybe SyncToken)
  , itemMetaData             :: !(Maybe ModificationMetaData)
  , itemName                 :: !QBText -- max 100
  , itemDescription          :: !(Maybe QBText) -- max 4000
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
  deriving (Eq, Ord, Show)

data Bundle = Bundle
  { bundleId                 :: !(Maybe QBText)
  , bundleSyncToken          :: !(Maybe SyncToken)
  , bundleMetaData           :: !(Maybe ModificationMetaData)
  , bundleName               :: !QBText -- max 100
  , bundleSKU                :: !(Maybe QBText)
  , bundleActive             :: !(Maybe Bool) -- Always true for categories
  , bundleDescription        :: !(Maybe QBText) -- max 4000
  , bundleFullyQualifiedName :: !(Maybe QBText)    -- readonly, system gen
  , bundleTaxable            :: !(Maybe Bool) -- US only
  , bundleUnitPrice          :: !(Maybe Double) -- max 99999999999, def 0
  , bundleType               :: !(Maybe Text) -- Set to "Group" for bundles
  , bundlePurchaseCost       :: !(Maybe Double) -- maximum of 99999999999
  , bundlePrintGroupItems    :: !(Maybe Bool) -- Specifies if all group items get printed
  , bundleGroupDetail        :: !(Maybe ItemGroupDetail) -- [ItemGroupLine]
  }
  deriving (Eq, Ord, Show)

data Category = Category
  { categoryId                 :: !(Maybe QBText)
  , categorySyncToken          :: !(Maybe SyncToken)
  , categoryMetaData           :: !(Maybe ModificationMetaData)
  , categoryName               :: !QBText -- max 100
  , categoryActive             :: !(Maybe Bool) -- Always true for categories
  , categorySubItem            :: !(Maybe Bool) -- true -> Sub-category, false -> top-level (default)
  , categoryParentRef          :: !(Maybe Reference) --
  , categoryLevel              :: !(Maybe Integer)   -- 0 to 3
  , categoryFullyQualifiedName :: !(Maybe QBText)    -- readonly
  , categoryType               :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show)

data DeletedCategory = DeletedCategory
  { deletedCategoryId     :: !Text
  , deletedCategorydomain :: !Text
  , deletedCategorystatus :: !Text
  } deriving (Show, Eq, Ord)

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
               { fieldLabelModifier = drop 6
               , omitNothingFields  = True
               }
             ''Bundle)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop 8
               , omitNothingFields  = True
               }
             ''Category)

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
               { fieldLabelModifier = drop (length ("itemGroupDetail" :: String)) }
             ''ItemGroupDetail)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop (length ("itemGroupLine" :: String)) }
             ''ItemGroupLine)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop (length ("deletedInvoice" :: String)) }
             ''DeletedInvoice)

$(deriveJSON defaultOptions
               { fieldLabelModifier = drop (length ("deletedCategory" :: String)) }
             ''DeletedCategory)
