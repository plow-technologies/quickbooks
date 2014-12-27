{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}

module QuickBooks
  ( createInvoice
  ) where 

import           QuickBooks.Types
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client
import           QuickBooks.Requests

createInvoice :: Invoice -> IO (Either String Invoice)
createInvoice = queryQuickBooks . CreateInvoice

queryQuickBooks :: QuickBooksQuery a -> IO (Either String a)
queryQuickBooks query = do
  apiConfig <- readAPIConfig
  manager   <- newManager tlsManagerSettings
  let ?apiConfig = apiConfig 
  let ?manager = manager
  case query of
    (CreateInvoice invoice) -> createInvoiceHandler invoice
    (UpdateInvoice _)       -> undefined
    (ReadInvoice _)         -> undefined
    (DeleteInvoice _)       -> undefined

readAPIConfig :: IO APIConfig
readAPIConfig = undefined
