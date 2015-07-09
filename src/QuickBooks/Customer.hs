{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Customer
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

module QuickBooks.Customer
  ( queryCustomerRequest
  )
  where

import QuickBooks.Types

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text

-- GET /v3/company/<companyID>/query=<selectStatement>

queryCustomerRequest
  :: ByteString
  -> Text
  -> IO (Either String (QuickBooksResponse Customer))
queryCustomerRequest _ displayName =
  undefined
  where
    query =
      Text.concat
        [ "select * from Customer Where DisplayName>'"
        , displayName
        ,  "' Order By DisplayName"
        ]
