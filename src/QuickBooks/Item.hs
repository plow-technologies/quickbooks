{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Item
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

module QuickBooks.Item
  ( queryItemRequest
  )
  where

import QuickBooks.Types

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text

-- GET /v3/company/<companyID>/query=<selectStatement>
-- select * from Item maxresults 2

queryItemRequest
  :: ByteString
  -> Text
  -> IO (Either String (QuickBooksResponse Item))
queryItemRequest _ name =
  undefined
  where
    query =
      Text.concat
        [ "select * from Item Where Name>'"
        , name
        ,  "' Order By Name"
        ]
