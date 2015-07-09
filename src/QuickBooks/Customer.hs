{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

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

import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types

import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept)

-- GET /v3/company/<companyID>/query=<selectStatement>

queryCustomerRequest
  :: ( ?apiConfig :: APIConfig
     , ?manager   :: Manager
     , ?logger    :: Logger
     )
  => OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Customer]))
queryCustomerRequest tok queryCustomerName = do
  let apiConfig = ?apiConfig
  let queryURI = parseUrl [i|#{queryURITemplate apiConfig}#{query}|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return (Aeson.eitherDecode (responseBody resp))
  where
    query =
      Text.concat
        [ "select * from Customer Where DisplayName>'"
        , queryCustomerName
        ,  "' Order By DisplayName"
        ]

queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query=|]
