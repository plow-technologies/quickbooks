{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

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

queryItemRequest
  :: ( ?apiConfig :: APIConfig
     , ?appConfig :: AppConfig
     , ?manager   :: Manager
     , ?logger    :: Logger
     )
  => OAuthToken
  -> Text
  -> IO (Either String (QuickBooksResponse [Item]))
queryItemRequest tok queryItemName = do
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
        [ "SELECT * FROM Item WHERE Name='"
        , queryItemName
        ,  "' ORDER BY Name"
        ]

queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]
