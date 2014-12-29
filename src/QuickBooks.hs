{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module QuickBooks
  ( createInvoice
  , readInvoice
  , updateInvoice
  , deleteInvoice
  ) where 

import Control.Applicative
import Control.Arrow (second)
import Data.ByteString.Char8 (pack)
import QuickBooks.Requests
import QuickBooks.Types
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client
import System.Environment (getEnvironment)

createInvoice :: Invoice -> IO (Either String Invoice)
createInvoice = queryQuickBooks . CreateInvoice

readInvoice :: InvoiceId -> IO (Either String Invoice)
readInvoice = queryQuickBooks . ReadInvoice

updateInvoice :: Invoice -> IO (Either String Invoice)
updateInvoice = queryQuickBooks . UpdateInvoice

deleteInvoice :: InvoiceId -> SyncToken -> IO (Either String Invoice)
deleteInvoice iId = queryQuickBooks . DeleteInvoice iId

queryQuickBooks :: QuickBooksQuery a -> IO (Either String a)
queryQuickBooks query = do
  apiConfig <- readAPIConfig
  manager   <- newManager tlsManagerSettings
  let ?apiConfig = apiConfig 
  let ?manager = manager
  case query of
    (CreateInvoice invoice) -> createInvoiceRequest invoice 
    (UpdateInvoice invoice) -> updateInvoiceRequest invoice
    (ReadInvoice invoiceId) -> readInvoiceRequest invoiceId
    (DeleteInvoice invoiceId syncToken) -> deleteInvoiceRequest invoiceId syncToken

readAPIConfig :: IO APIConfig
readAPIConfig = do
  env <- getEnvironment
  case lookupAPIConfig env of
    Just config -> return config
    Nothing     -> fail "INTUIT_COMPANY_ID,INTUIT_CONSUMER_KEY,INTUIT_CONSUMER_SECRET,INTUIT_TOKEN,INTUIT_SECRET,INTUIT_HOSTNAME must be set"

lookupAPIConfig :: [(String, String)] -> Maybe APIConfig
lookupAPIConfig environment = APIConfig <$> lookup "INTUIT_COMPANY_ID" env
                                        <*> lookup "INTUIT_CONSUMER_KEY" env
                                        <*> lookup "INTUIT_CONSUMER_SECRET" env
                                        <*> lookup "INTUIT_TOKEN" env
                                        <*> lookup "INTUIT_SECRET" env
                                        <*> lookup "INTUIT_HOSTNAME" env
    where env = map (second pack) environment
