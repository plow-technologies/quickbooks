{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}

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
  , countItemRequest
  , queryMaxItemRequest
  , createItemRequest
  , readItemRequest
  , updateItemRequest
  , deleteItemRequest
  )
  where

import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types

import Data.Aeson                (encode, eitherDecode)
import Data.String.Interpolate   (i)
import Data.Text                 (Text)
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept, hContentType)
import Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)

-- | Create an item. (Supply a new Item WITHOUT an id field)
createItemRequest :: APIEnv
                     => OAuthToken
                     -> Item
                     -> IO (Either String (QuickBooksResponse [Item]))
createItemRequest tok = postItem tok

-- | Read an item by id
readItemRequest ::  APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Item]))
readItemRequest tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{itemURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- | Update an item.  (Supply a new item with the same id as the old item to replace the fields in it)
updateItemRequest :: APIEnv
                     => OAuthToken
                     -> Item
                     -> IO (Either String (QuickBooksResponse [Item]))
updateItemRequest tok = postItem tok

-- | Delete an item.  ( In QuickBooks the items can not actually be deleted
--                      Instead they are set to inactive and thus hidden from the user )
deleteItemRequest :: APIEnv
                     => OAuthToken
                     -> Item
                     -> IO (Either String (QuickBooksResponse [Item]))
deleteItemRequest tok cItem = do
  let nItem = cItem { itemActive = (Just False) }
  postItem tok nItem


-- Post handles create/update/delete in the api
postItem :: APIEnv
            => OAuthToken
            -> Item
            -> IO (Either String (QuickBooksResponse [Item]))
postItem tok item = do
  let apiConfig = ?apiConfig
  req <- parseUrlThrow $ [i|#{itemURITemplate apiConfig}?|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode item
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp



-- GET /v3/company/<companyID>/query=<selectStatement>

-- Searches by name
queryItemRequest :: APIEnv
                 => OAuthToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Item]))
queryItemRequest tok queryItemName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{itemSearch}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundItems = eitherDecode (responseBody resp)
  case eitherFoundItems of
    Left er -> return (Left er)
    Right (QuickBooksItemResponse foundItems) ->
      return $ Right $ QuickBooksItemResponse $ foundItems
        -- filter (\Item{..} -> itemName == queryItemName) FoundItems
  where
    query :: String
    query = "SELECT * FROM Item"
    -- if Text /= "" -> Where Name='input'
    -- if Text == "" -> return all items
    itemName = [i|#{queryItemName}|]
    itemSearch :: String
    itemSearch = if (itemName == "")
      then ""                                  -- All Items
      else [i| Where Name='#{queryItemName}'|] -- Item that Matchs Name


queryMaxItemRequest :: APIEnv
                 => OAuthToken
                 -> Int
                 -> IO (Either String (QuickBooksResponse [Item]))
queryMaxItemRequest tok startIndex = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query} #{pagination}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundItems = eitherDecode (responseBody resp)
  case eitherFoundItems of
    Left er -> return (Left er)
    Right (QuickBooksItemResponse foundItems) ->
      return $ Right $ QuickBooksItemResponse $ foundItems
        -- filter (\Item{..} -> itemName == queryItemName) FoundItems
  where
    query :: String
    query = "SELECT * FROM Item"
    pagination :: String
    pagination = [i| startposition #{startIndex} maxresults 1000|] -- Item that Matchs Name

countItemRequest :: APIEnv
                 => OAuthToken
                 -> IO (Either String (QuickBooksResponse Int))
countItemRequest tok = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundCount = eitherDecode (responseBody resp)
  case eitherFoundCount of
    Left er -> return (Left er)
    Right (QuickBooksCountResponse foundCount) ->
      return $ Right $ QuickBooksCountResponse $ foundCount
        -- filter (\Item{..} -> itemName == queryItemName) FoundItems
  where
    query :: String
    query = "SELECT COUNT(*) FROM Item"


-- Template for queries
queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]

-- Template for Item API Calls
--   / for read
--   ? update create 'delete'
itemURITemplate :: APIConfig -> String
itemURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/item|]
