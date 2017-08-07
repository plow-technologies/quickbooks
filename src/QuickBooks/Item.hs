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
import URI.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy (fromStrict)
import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types
import qualified Network.OAuth.OAuth2            as OAuth2
import Data.Aeson                (encode, eitherDecode)
import Data.String.Interpolate   (i)
import Data.Text                 (Text)
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept, hContentType)
import Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)

-- | Create an item. (Supply a new Item WITHOUT an id field)
createItemRequest :: APIEnv
                     => OAuthTokens
                     -> Item
                     -> IO (Either String (QuickBooksResponse [Item]))
createItemRequest tok = postItem tok


readItemRequest ::  APIEnv
                => OAuthTokens
                -> Text
                -> IO (Either String (QuickBooksResponse [Item]))
readItemRequest (OAuth1 tok) iId = readItemRequestOAuth tok iId
readItemRequest (OAuth2 tok) iId = readItemRequestOAuth2 tok iId

readItemRequestOAuth ::  APIEnv
                    => OAuthToken
                    -> Text
                    -> IO (Either String (QuickBooksResponse [Item]))
readItemRequestOAuth tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{itemURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- | Read an item by id
readItemRequestOAuth2 ::  APIEnv
                     => OAuth2.AccessToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Item]))
readItemRequestOAuth2 tok iId = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{itemURITemplate apiConfig}/#{iId}|]
  req' <- parseUrlThrow (escapeURIString isUnescapedInURI [i|#{itemURITemplate apiConfig}/#{iId}|])
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp

-- | Update an item.  (Supply a new item with the same id as the old item to replace the fields in it)
updateItemRequest :: APIEnv
                     => OAuthTokens
                     -> Item
                     -> IO (Either String (QuickBooksResponse [Item]))
updateItemRequest tok = postItem tok

-- | Delete an item.  ( In QuickBooks the items can not actually be deleted
--                      Instead they are set to inactive and thus hidden from the user )
deleteItemRequest :: APIEnv
                     => OAuthTokens
                     -> Item
                     -> IO (Either String (QuickBooksResponse [Item]))
deleteItemRequest tok cItem = do
  let nItem = cItem { itemActive = (Just False) }
  postItem tok nItem


-- Post handles create/update/delete in the api
postItem :: APIEnv =>
            OAuthTokens ->
            Item ->
            IO (Either String (QuickBooksResponse [Item]))
postItem (OAuth1 tok) item = postItemOAuth tok item
postItem (OAuth2 tok) item = postItemOAuth2 tok item

-- Post handles create/update/delete in the api
postItemOAuth :: APIEnv =>
               OAuthToken ->
               Item ->
               IO (Either String (QuickBooksResponse [Item]))
postItemOAuth tok item = do
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




postItemOAuth2 :: APIEnv
                  => OAuth2.AccessToken
                  -> Item
                  -> IO (Either String (QuickBooksResponse [Item]))
postItemOAuth2 tok item = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{itemURITemplate apiConfig}?|]
  -- Made for providing an error log
  req' <- parseUrlThrow $ [i|#{itemURITemplate apiConfig}?|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI item
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp

-- GET /v3/company/<companyID>/query=<selectStatement>

----- Query Item Request -----
   -- Searches by name
queryItemRequest :: APIEnv
                 => OAuthTokens
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Item]))
queryItemRequest (OAuth1 tok) searchName = queryItemRequestOAuth tok searchName
queryItemRequest (OAuth2 tok) searchName = queryItemRequestOAuth2 tok searchName

--- OAuth 1 ---
queryItemRequestOAuth :: APIEnv
                 => OAuthToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Item]))
queryItemRequestOAuth tok queryItemName = do
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

--- OAuth 2 ---
queryItemRequestOAuth2 :: APIEnv
                      => OAuth2.AccessToken
                      -> Text
                      -> IO (Either String (QuickBooksResponse [Item]))
queryItemRequestOAuth2 tok queryItemName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{itemSearch}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  -- Made for providing an error log
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          let eitherFoundItems = eitherDecode resp
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

----- Query Max Items -----
queryMaxItemRequest :: APIEnv
                    => OAuthTokens
                    -> Int
                    -> IO (Either String (QuickBooksResponse [Item]))
queryMaxItemRequest (OAuth1 tok) index = queryMaxItemRequestOAuth tok index
queryMaxItemRequest (OAuth2 tok) index = queryMaxItemRequestOAuth2 tok index

--- OAuth 1 ---
queryMaxItemRequestOAuth :: APIEnv
                        => OAuthToken
                        -> Int
                        -> IO (Either String (QuickBooksResponse [Item]))
queryMaxItemRequestOAuth tok startIndex = do
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

--- OAuth 2 ---
queryMaxItemRequestOAuth2 :: APIEnv
                          => OAuth2.AccessToken
                          -> Int
                          -> IO (Either String (QuickBooksResponse [Item]))
queryMaxItemRequestOAuth2 tok startIndex = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query} #{pagination}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  -- Made for providing an error log
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          let eitherFoundItems = eitherDecode resp
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







----- Count Items -----
countItemRequest :: APIEnv
                 => OAuthTokens
                 -> IO (Either String (QuickBooksResponse Int))
countItemRequest (OAuth1 tok) = countItemRequestOAuth tok
countItemRequest (OAuth2 tok) = countItemRequestOAuth2 tok

--- OAuth 1 ---
countItemRequestOAuth :: APIEnv =>
                         OAuthToken ->
                         IO (Either String (QuickBooksResponse Int))
countItemRequestOAuth tok = do
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

--- OAuth 2 ---
countItemRequestOAuth2 :: APIEnv =>
                          OAuth2.AccessToken ->
                          IO (Either String (QuickBooksResponse Int))
countItemRequestOAuth2 tok = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  -- Made for providing an error log
  req' <- parseUrlThrow [i|#{queryURITemplate apiConfig}#{uriComponent}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
           let eitherFoundCount = eitherDecode resp
           case eitherFoundCount of
             Left er -> return (Left er)
             Right (QuickBooksCountResponse foundCount) ->
               return $ Right $ QuickBooksCountResponse $ foundCount
                  -- filter (\Item{..} -> itemName == queryItemName) FoundItems
  where
    query :: String
    query = "SELECT COUNT(*) FROM Item"

----- Templates -----

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
