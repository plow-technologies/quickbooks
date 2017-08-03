{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}

------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Category
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

module QuickBooks.Category
  ( queryCategoryRequest
  , queryMaxCategoryRequest
  , countCategoryRequest
  , createCategoryRequest
  , readCategoryRequest
  , updateCategoryRequest
  , deleteCategoryRequest
  )
  where

import QuickBooks.Authentication
import QuickBooks.Logging
import QuickBooks.Types
-- import QuickBooks.QBText
import qualified Network.OAuth.OAuth2      as OAuth2
import           Data.ByteString.Char8
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Aeson                (encode, eitherDecode)
import           Data.String.Interpolate   (i)
import           Data.Text                 (Text)
import           Network.HTTP.Client
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.URI               (escapeURIString, isUnescapedInURI, isUnescapedInURIComponent)
import           URI.ByteString

-- | Create an category. (Supply a new Category WITHOUT an id field)
createCategoryRequest :: APIEnv
                     => OAuthTokens
                     -> Category
                     -> IO (Either String (QuickBooksResponse [Category]))
createCategoryRequest tok = postCategory tok

-- | Read an category by id
readCategoryRequest ::  APIEnv
                     => OAuthTokens
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Category]))
readCategoryRequest (OAuth1 tok) iId = readCategoryRequestOAuth tok iId
readCategoryRequest (OAuth2 tok) iId = readCategoryRequestOAuth2 tok iId

--- OAuth 1 ---
readCategoryRequestOAuth ::  APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Category]))
readCategoryRequestOAuth tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
readCategoryRequestOAuth2 ::  APIEnv
                     => OAuth2.AccessToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Category]))
readCategoryRequestOAuth2 tok iId = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{categoryURITemplate apiConfig}/#{iId}|]
  -- Made for logging
  req'  <- parseUrlThrow (escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}/#{iId}|])
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

-- | Update an category.  (Supply a new category with the same id as the old category to replace the fields in it)
updateCategoryRequest :: APIEnv
                     => OAuthTokens
                     -> Category
                     -> IO (Either String (QuickBooksResponse [Category]))
updateCategoryRequest tok = postCategory tok

-- | Delete an category.  ( In QuickBooks the categorys can not actually be deleted
--                      Instead they are set to inactive and thus hidden from the user )
deleteCategoryRequest :: APIEnv
                     => OAuthTokens
                     -> Category
                     -> IO (Either String (QuickBooksResponse DeletedCategory))
deleteCategoryRequest (OAuth1 tok) category = deleteCategoryRequestOAuth tok category
deleteCategoryRequest (OAuth2 tok) category = deleteCategoryRequestOAuth2 tok category

--- OAuth 1 ---
deleteCategoryRequestOAuth :: APIEnv
                     => OAuthToken
                     -> Category
                     -> IO (Either String (QuickBooksResponse DeletedCategory))
deleteCategoryRequestOAuth tok cCategory = do
  let apiConfig = ?apiConfig
  req <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}?operation=delete&minorversion=4|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode cCategory
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
deleteCategoryRequestOAuth2 :: APIEnv
                     => OAuth2.AccessToken
                     -> Category
                     -> IO (Either String (QuickBooksResponse DeletedCategory))
deleteCategoryRequestOAuth2 tok cCategory = do
  let apiConfig = ?apiConfig
  -- Made for logging
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{categoryURITemplate apiConfig}?operation=delete&minorversion=4|]
  req' <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}?operation=delete&minorversion=4|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI cCategory
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp


-- Post handles create/update in the api
postCategory :: APIEnv
            => OAuthTokens
            -> Category
            -> IO (Either String (QuickBooksResponse [Category]))
postCategory (OAuth1 tok) category = postCategoryOAuth tok category
postCategory (OAuth2 tok) category = postCategoryOAuth2 tok category

--- OAuth 1 ---
postCategoryOAuth :: APIEnv
            => OAuthToken
            -> Category
            -> IO (Either String (QuickBooksResponse [Category]))
postCategoryOAuth tok category = do
  let apiConfig = ?apiConfig
  req <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}?minorversion=4|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode category
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
postCategoryOAuth2 :: APIEnv
            => OAuth2.AccessToken
            -> Category
            -> IO (Either String (QuickBooksResponse [Category]))
postCategoryOAuth2 tok category = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{categoryURITemplate apiConfig}?minorversion=4|]
  -- Made for logging
  req' <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}?minorversion=4|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI category
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp

-- GET /v3/company/<companyID>/query=<selectStatement>

-- Searches by name
queryCategoryRequest :: APIEnv
                 => OAuthTokens
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Category]))
queryCategoryRequest (OAuth1 tok) queryCategoryName = queryCategoryRequestOAuth tok queryCategoryName
queryCategoryRequest (OAuth2 tok) queryCategoryName = queryCategoryRequestOAuth2 tok queryCategoryName

--- OAuth 1 ---
queryCategoryRequestOAuth :: APIEnv
                 => OAuthToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Category]))
queryCategoryRequestOAuth tok queryCategoryName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{categorySearch}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundCategorys = eitherDecode (responseBody resp)
  case eitherFoundCategorys of
    Left er -> return (Left er)
    Right (QuickBooksCategoryResponse foundCategorys) ->
      return $ Right $ QuickBooksCategoryResponse $ foundCategorys
        -- filter (\Category{..} -> categoryName == queryCategoryName) FoundCategorys
  where
    query :: String
    query = "SELECT * FROM Item"
    -- if Text /= "" -> Where Name='input'
    -- if Text == "" -> return all categorys
    categoryName = [i|#{queryCategoryName}|]
    categorySearch :: String
    categorySearch = if (categoryName == "")
      then " where Type='Category'" -- All Categorys
      else [i| WHERE Type='Category' AND Name='#{queryCategoryName}'|]    -- Category that Matchs Name

--- OAuth 2 ---
queryCategoryRequestOAuth2 :: APIEnv
                 => OAuth2.AccessToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Category]))
queryCategoryRequestOAuth2 tok queryCategoryName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{categorySearch}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  -- Made for logging
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          let eitherFoundCategorys = eitherDecode resp
          case eitherFoundCategorys of
            Left er -> return (Left er)
            Right (QuickBooksCategoryResponse foundCategorys) ->
              return $ Right $ QuickBooksCategoryResponse $ foundCategorys
                -- filter (\Category{..} -> categoryName == queryCategoryName) FoundCategorys
  where
    query :: String
    query = "SELECT * FROM Item"
    -- if Text /= "" -> Where Name='input'
    -- if Text == "" -> return all categorys
    categoryName = [i|#{queryCategoryName}|]
    categorySearch :: String
    categorySearch = if (categoryName == "")
      then " where Type='Category'" -- All Categorys
      else [i| WHERE Type='Category' AND Name='#{queryCategoryName}'|]    -- Category that Matchs Name




--- Queries the max (1000) from a starting index ---
queryMaxCategoryRequest :: APIEnv
                 => OAuthTokens
                 -> Int
                 -> IO (Either String (QuickBooksResponse [Category]))
queryMaxCategoryRequest (OAuth1 tok) startIndex = queryMaxCategoryRequestOAuth tok startIndex
queryMaxCategoryRequest (OAuth2 tok) startIndex = queryMaxCategoryRequestOAuth2 tok startIndex


--- OAuth 1 ---
queryMaxCategoryRequestOAuth :: APIEnv
                 => OAuthToken
                 -> Int
                 -> IO (Either String (QuickBooksResponse [Category]))
queryMaxCategoryRequestOAuth tok startIndex = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query} #{pagination}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  req <- oauthSignRequest tok =<< queryURI
  let oauthHeaders = requestHeaders req
  let req' = req { method = "GET"
                 , requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]
                 }
  resp <- httpLbs req' ?manager
  logAPICall req'
  let eitherFoundCategories = eitherDecode (responseBody resp)
  case eitherFoundCategories of
    Left er -> return (Left er)
    Right (QuickBooksCategoryResponse foundCategories) ->
      return $ Right $ QuickBooksCategoryResponse $ foundCategories
        -- filter (\Category{..} -> itemName == queryCategoryName) FoundCategories
  where
    query :: String
    query = "SELECT * FROM Item WHERE Type='Category'"
    pagination :: String
    pagination = [i| startposition #{startIndex} maxresults 1000|] -- Category that Matchs Name

--- OAuth 2 ---
queryMaxCategoryRequestOAuth2 :: APIEnv
                 => OAuth2.AccessToken
                 -> Int
                 -> IO (Either String (QuickBooksResponse [Category]))
queryMaxCategoryRequestOAuth2 tok startIndex = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query} #{pagination}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  -- Made for logging
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthGetBS ?manager tok queryURI
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          let eitherFoundCategories = eitherDecode resp
          case eitherFoundCategories of
            Left er -> return (Left er)
            Right (QuickBooksCategoryResponse foundCategories) ->
              return $ Right $ QuickBooksCategoryResponse $ foundCategories
                -- filter (\Category{..} -> itemName == queryCategoryName) FoundCategories
  where
    query :: String
    query = "SELECT * FROM Item WHERE Type='Category'"
    pagination :: String
    pagination = [i| startposition #{startIndex} maxresults 1000|] -- Category that Matchs Name


----- Counts the number of categories ------
countCategoryRequest :: APIEnv
                 => OAuthTokens
                 -> IO (Either String (QuickBooksResponse Int))
countCategoryRequest (OAuth1 tok) = countCategoryRequestOAuth tok
countCategoryRequest (OAuth2 tok) = countCategoryRequestOAuth2 tok

--- OAuth 1 ---
countCategoryRequestOAuth :: APIEnv
                 => OAuthToken
                 -> IO (Either String (QuickBooksResponse Int))
countCategoryRequestOAuth tok = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let queryURI = parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
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
        -- filter (\Category{..} -> itemName == queryCategoryName) FoundCategories
  where
    query :: String
    query = "SELECT COUNT(*) FROM Item WHERE Type='Category'"

--- OAuth 2 ---
countCategoryRequestOAuth2 :: APIEnv
                 => OAuth2.AccessToken
                 -> IO (Either String (QuickBooksResponse Int))
countCategoryRequestOAuth2 tok = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}|]
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
  -- Made for logging
  req' <- parseUrlThrow $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
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
                -- filter (\Category{..} -> itemName == queryCategoryName) FoundCategories
  where
    query :: String
    query = "SELECT COUNT(*) FROM Item WHERE Type='Category'"


-- Template for queries
queryURITemplate :: APIConfig -> String
queryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/query?query=|]

-- Template for Category API Calls
--   / for read
--   ? update create 'delete'
categoryURITemplate :: APIConfig -> String
categoryURITemplate APIConfig{..} =
  [i|https://#{hostname}/v3/company/#{companyId}/item|]
