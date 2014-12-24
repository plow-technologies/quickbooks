{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}

module QuickBooks where

import           QuickBooks.Types

import           Blaze.ByteString.Builder
import           Control.Applicative ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (def)
import           Data.Text (Text)
import           Network.HTTP.Types.URI (encodePathSegments)
import           Network.Wreq (delete, get, post)
import           Network.Wreq.Types
import qualified Network.Wreq.Session as WS
import           Web.Authenticate.OAuth hiding (delete)

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
  { companyId      :: !Text
  , consumerToken  :: !Text
  , consumerSecret :: !Text
  , oauthToken     :: !Text
  , oauthSecret    :: !Text
  , hostname       :: !Text
  }


quickbooksOAuth :: OAuth
quickbooksOAuth = undefined

runWithOAuthSignature :: APIConfig -> WS.Session -> Run a -> Run a
runWithOAuthSignature apiConfig (WS.Session{..}) act (Req _ req) = undefined

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
