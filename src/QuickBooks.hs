{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}

module QuickBooks
  ( createInvoice
  , readInvoice
  , updateInvoice
  , deleteInvoice
  ) where 

import           QuickBooks.Types
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client
import           QuickBooks.Requests

createInvoice :: Invoice -> IO (Either String Invoice)
createInvoice = queryQuickBooks . CreateInvoice

readInvoice :: InvoiceId -> IO (Either String Invoice)
readInvoice = queryQuickBooks . ReadInvoice

updateInvoice :: Invoice -> IO (Either String Invoice)
updateInvoice = queryQuickBooks . UpdateInvoice

deleteInvoice :: InvoiceId -> IO (Either String Invoice)
deleteInvoice = queryQuickBooks . DeleteInvoice

queryQuickBooks :: QuickBooksQuery a -> IO (Either String a)
queryQuickBooks query = do
  apiConfig <- readAPIConfig
  manager   <- newManager tlsManagerSettings
  let ?apiConfig = apiConfig 
  let ?manager = manager
  case query of
    (CreateInvoice invoice)   -> createInvoiceRequest invoice 
    (UpdateInvoice invoice)   -> updateInvoiceRequest invoice
    (ReadInvoice invoiceId)   -> readInvoiceRequest invoiceId
    (DeleteInvoice invoiceId) -> deleteInvoiceRequest invoiceId 

readAPIConfig :: IO APIConfig
readAPIConfig = undefined
