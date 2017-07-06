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
  , createCategoryRequest
  , readCategoryRequest
  , updateCategoryRequest
  , deleteCategoryRequest
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

-- | Create an category. (Supply a new Category WITHOUT an id field)
createCategoryRequest :: APIEnv
                     => OAuthToken
                     -> Category
                     -> IO (Either String (QuickBooksResponse [Category]))
createCategoryRequest tok = postCategory tok

-- | Read an category by id
readCategoryRequest ::  APIEnv
                     => OAuthToken
                     -> Text
                     -> IO (Either String (QuickBooksResponse [Category]))
readCategoryRequest tok iId = do
  let apiConfig = ?apiConfig
  req  <- oauthSignRequest tok =<< parseUrl (escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}/#{iId}|])
  let oauthHeaders = requestHeaders req
  let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
  resp <-  httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- | Update an category.  (Supply a new category with the same id as the old category to replace the fields in it)
updateCategoryRequest :: APIEnv
                     => OAuthToken
                     -> Category
                     -> IO (Either String (QuickBooksResponse [Category]))
updateCategoryRequest tok = postCategory tok

-- | Delete an category.  ( In QuickBooks the categorys can not actually be deleted
--                      Instead they are set to inactive and thus hidden from the user )
deleteCategoryRequest :: APIEnv
                     => OAuthToken
                     -> Category
                     -> IO (Either String (QuickBooksResponse DeletedCategory))
deleteCategoryRequest tok cCategory = do
  let apiConfig = ?apiConfig
  req <- parseUrl $ escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}?operation=delete&minorversion=4|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode cCategory
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

-- Post handles create/update/delete in the api
postCategory :: APIEnv
            => OAuthToken
            -> Category
            -> IO (Either String (QuickBooksResponse [Category]))
postCategory tok category = do
  let apiConfig = ?apiConfig
  req <- parseUrl $ escapeURIString isUnescapedInURI [i|#{categoryURITemplate apiConfig}?minorversion=4|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode category
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp



-- GET /v3/company/<companyID>/query=<selectStatement>

-- Searches by name
queryCategoryRequest :: APIEnv
                 => OAuthToken
                 -> Text
                 -> IO (Either String (QuickBooksResponse [Category]))
queryCategoryRequest tok queryCategoryName = do
  let apiConfig = ?apiConfig
  let uriComponent = escapeURIString isUnescapedInURIComponent [i|#{query}#{categorySearch}|]
  let queryURI = parseUrl $ [i|#{queryURITemplate apiConfig}#{uriComponent}&minorversion=4|]
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
      then "where Type='Category'" -- All Categorys
      else [i| WHERE Type='Category' AND Name='#{queryCategoryName}'|]    -- Category that Matchs Name

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
