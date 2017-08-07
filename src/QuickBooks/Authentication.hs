{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}



{- |
Module      : QuickBooks.Authentication
Description : Module for gaining Access Tokens, OAuth and OAuth2
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


https://developer.intuit.com/docs/0100_quickbooks_online/0100_essentials/000500_authentication_and_authorization/implement_single_sign-on_with_openid#/Initiating_the_authentication_request

https://developer.intuit.com/docs/0100_quickbooks_online/0100_essentials/000500_authentication_and_authorization/implement_single_sign-on_with_openid#/Discovery_document




-}

module QuickBooks.Authentication 
  ( getTempOAuthCredentialsRequest
  , getAccessTokenRequest
  , oauthSignRequest
  , authorizationURLForToken
  , disconnectRequest
  , qbAuthGetBS
  , qbAuthPostBS
  , fetchAccessToken
  , readOAuth2Config
  , makeOAuth2
 ) where

import Control.Monad (void, liftM, ap)
import Data.Monoid ((<>))
import Data.Yaml (ParseException, decodeFileEither)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (unpack, ByteString)
import Network.HTTP.Client (Manager
                            ,Request(..)
                             ,RequestBody(RequestBodyLBS,RequestBodyBS)
                             ,requestBody
                             ,responseBody
                             ,parseUrlThrow
                             ,setQueryString
                            ,getUri
                            ,httpLbs)

import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.URI               (escapeURIString
                                 , isUnescapedInURI)

import qualified Network.HTTP.Client.TLS as TLS

import URI.ByteString (urlDecodeQuery,serializeURIRef',URI)
import URI.ByteString.QQ
import Web.Authenticate.OAuth (signOAuth
                              ,newCredential
                              ,emptyCredential
                              ,injectVerifier
                              ,newOAuth
                              ,OAuth(..))
                              

import           Data.Aeson 
import           Data.Text.Encoding (encodeUtf8)
import           QuickBooks.Logging (logAPICall')
import           QuickBooks.Types
import qualified Network.OAuth.OAuth2            as OAuth2
import qualified Network.OAuth.OAuth2.HttpClient as OAuth2
import qualified Network.OAuth.OAuth2.Internal   as OAuth2
import qualified Network.HTTP.Types            as HT



qbAuthGetBS ::  Manager	-> OAuth2.AccessToken	 
            -> URI	 
            -> IO (OAuth2.OAuth2Result String BSL.ByteString)
qbAuthGetBS = OAuth2.authGetBS


-- | Conduct POST request for Quickbooks.

qbAuthPostBS ::ToJSON a =>  Manager	-> OAuth2.AccessToken	 
            -> URI
            -> a
            -> IO (OAuth2.OAuth2Result String BSL.ByteString)
qbAuthPostBS manager token url pb = do
  req <- OAuth2.uriToRequest url
  let (RequestBodyBS bs) = requestBody $ upBody req
  
  OAuth2.authRequest req upReq manager
  where upBody req = req {requestBody =  RequestBodyBS $ BSL.toStrict $ encode pb }        
        upHeaders  = OAuth2.updateRequestHeaders (Just token) . OAuth2.setMethod HT.POST
        upContentHeader req = req {requestHeaders = ((HT.hContentType,"application/json") : (requestHeaders req) )}
        upReq      = upContentHeader . upBody . upHeaders

testValue = eitherDecode "This isn't valid" :: Either String Value 
-- {
--   "SyncToken":"0",
--   "Active":false,
--   "Level":0,
--   "UnitPrice":0,
--   "Type":"Inventory",
--   "IncomeAccountRef":{
--     "name":"Sales of Product Income",
--     "value":"79"
--   },
--   "PurchaseDesc":"Purchase Desc",
--   "PurchaseCost":0,
--   "ExpenseAccountRef":{
--     "name":"Cost of Goods Sold",
--     "value":"80"
--   },
--   "AssetAccountRef":{
--     "name":"Inventory Asset",
--     "value":"81"
--   },
--   "TrackQtyOnHand":true,
--   "QtyOnHand":10,
--   "InvStartDate":"2015-01-01",
--   "Name":"testItemName17238491529754203"
-- }


-- POST /v3/company/123145873160324/item?minorversion=4 HTTP/1.1
-- Host: sandbox-quickbooks.api.intuit.com
-- Authorization: Bearer eyJlbmMiOiJBMTI4Q0JDLUhTMjU2IiwiYWxnIjoiZGlyIn0..s_B3fFmr5mtt1FWU8QKmGA.JIFda4iB3amwDuulDgq_suCRdWFBDw1ceZwxufgL7XicY3eeT0DNZE5V7mRhTKCgbMmK7ebocqNCSYApEVUnZF_uwgjxo9BPT4i8mmkPYRztsL9Yy1vDLcq1MIH3GLkTZkcNH5E6L_nuA-PuE6U-oahA_YHAT__lBEjoy7Wn7C80gYVtLeTk41AMhsi5Gyh37lE2XeCRo35GtVE4f-BqHEWC6pPG4m6GguvYbuZZI8qhtRCWGHsO2wsYRTJReX6XzozvXHflioX7PmO2GBHrXfv6piyTSgr8TLZAKrX0FXpPxc-ZuwE3j7cSrK3MULeBpy3EFmUilfOUv4GzCk3V-5RXx-I49Wc18WvwxRRL3VaR-CDRZ8HuTlH3c4DuU87mZa-bwqroJ1qhKcLnac5n-1TBl1pE711MJYsPf1Hiz3Q2z8MbB7y3860IlHDNFelKNbpwLa2ovEaOVvlcYJDYyWgzouTaFIS5WpxN38zEyobJVgkFbeol6_g6rfEx8mUL8JwinIQgH5Hac5JyCubptaBxCT78EY5thWauvGt1zS1dPqdvNO368bkQsZ29Jy_vIh3aJfjfp2coI9zaxLKPowzDfnnvrvwvLqqWmUte_VIFB406K5VKRSBTzqNyQg9JPs_4ySmMUFgidEfz-cRWe77rcYCJSEoIo0Rr-NFHZ9SW2hK3g-MsUEexjOpuQSHU.mHB3E3GluIzfI8PPxxtw3Q
-- Content-Type: application/json
-- Accept: application/json
-- Cache-Control: no-cache
-- Postman-Token: 09a46062-bd6d-47c1-3f0e-7e2a68bdbb08



















--------------------------------------------------
-- OAUTH2
--------------------------------------------------
fetchAccessToken :: OAuth2Config -> IO OAuth2.OAuth2Token
fetchAccessToken oauth2Config = do
   mgr <- TLS.getGlobalManager
   let newOAuth2 = makeOAuth2 oauth2Config
   let refreshToken = OAuth2.RefreshToken $ oauthRefreshToken oauth2Config
   oauthTokenRslt <- OAuth2.fetchRefreshToken mgr newOAuth2 refreshToken
   case oauthTokenRslt of
     Left e       -> fail $ show e
     Right tok  -> do
       return tok

readOAuth2Config :: IO (Either String OAuth2Config)
readOAuth2Config = do
  eitherOAuth2Config <- readOAuth2ConfigFromFile $ "config/quickbooksConfig.yml"
  case eitherOAuth2Config of
    Left _ -> return $ Left "The config variables oauth2ClientId, oauth2ClientSecret, and oauth2RefreshToken must be set"
    Right config -> return $ Right config

readOAuth2ConfigFromFile :: FilePath -> IO (Either ParseException OAuth2Config)
readOAuth2ConfigFromFile = decodeFileEither

makeOAuth2 :: OAuth2Config -> OAuth2.OAuth2
makeOAuth2 config = OAuth2.OAuth2 {
    OAuth2.oauthClientId            = (oauthClientId config)
  , OAuth2.oauthClientSecret        = (oauthClientSecret config)
  , OAuth2.oauthOAuthorizeEndpoint  = [uri|https://appcenter.intuit.com/connect/obbauth2|]
  , OAuth2.oauthAccessTokenEndpoint = [uri|https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer|] 
  , OAuth2.oauthCallback            =  Just [uri|https://developer.intuit.com/v2/OAuth2Playground/RedirectUrl|]
  }

testOAuth2 :: OAuth2.OAuth2
testOAuth2 = OAuth2.OAuth2 {
    OAuth2.oauthClientId            = ""
  , OAuth2.oauthClientSecret        = ""
  , OAuth2.oauthOAuthorizeEndpoint  = [uri|https://appcenter.intuit.com/connect/oauth2|]
  , OAuth2.oauthAccessTokenEndpoint = [uri|https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer|] 
  , OAuth2.oauthCallback            =  Just [uri|https://developer.intuit.com/v2/OAuth2Playground/RedirectUrl|]
  }




-- Need to add something on the actual page to enable the token 
quickbooksAuthRequest oauth = do
            req <- parseUrlThrow "https://appcenter.intuit.com/connect/oauth2"
            mgr <- TLS.getGlobalManager
            let newReq = setQueryString parameters req 
--           return $ (getUri newReq)
            httpLbs newReq mgr

  where
    parameters = [
       ("client_id"    , Just $ encodeUtf8 . OAuth2.oauthClientId $ oauth )
      ,("scope"        , Just ".intuit.quickbooks.accounting openid email profile")
      ,("redirect_uri" , fmap serializeURIRef' . OAuth2.oauthCallback $ oauth)
      ,("response_type", Just "code")
      ,("state"        , Just "PlaygroundAuth")]









--------------------------------------------------
-- OAUTH
--------------------------------------------------

getTempOAuthCredentialsRequest :: AppEnv
                               => CallbackURL
                               -> IO (Either String (QuickBooksResponse OAuthToken)) -- ^ Temporary OAuthToken
getTempOAuthCredentialsRequest callbackURL =
  return . handleQuickBooksTokenResponse "Couldn't get temporary tokens" =<< tokensRequest

    where
      tokensRequest = getToken temporaryTokenURL "?oauth_callback=" callbackURL oauthSignRequestWithEmptyCredentials


getAccessTokenRequest :: AppEnv
                       => OAuthToken                                         -- ^ Temporary Token
                       -> OAuthVerifier                                      -- ^ OAuthVerifier provided by QuickBooks
                       -> IO (Either String (QuickBooksResponse OAuthToken)) -- ^ OAuthToken                                                                                 
getAccessTokenRequest  tempToken verifier =
  return.handleQuickBooksTokenResponse "Couldn't get access tokens" =<< tokensRequest
  where
    tokensRequest = getToken accessTokenURL "?oauth_token=" (unpack $ token tempToken) (oauthSignRequestWithVerifier verifier tempToken)


disconnectRequest :: AppEnv
                  => OAuthToken
                  -> IO (Either String (QuickBooksResponse ()))
disconnectRequest  tok = do
  req  <- parseUrlThrow $ escapeURIString isUnescapedInURI $ disconnectURL
  req' <- oauthSignRequest tok req
  void $ httpLbs req' ?manager
  logAPICall' req'
  return $ Right QuickBooksVoidResponse
    
getToken :: AppEnv
          => String                  -- ^ Endpoint to request the token
          -> String                  -- ^ URL parameter name
          -> String                  -- ^ URL parameter value
          -> ((?appConfig :: AppConfig) => Request -> IO Request) -- ^ Signing function
          -> IO (Maybe OAuthToken)
getToken tokenURL parameterName parameterValue signRequest = do
  request  <- parseUrlThrow $ escapeURIString isUnescapedInURI $ concat [tokenURL, parameterName, parameterValue]
  request' <- signRequest request { method="POST", requestBody = RequestBodyLBS "" }
  response <- httpLbs request' ?manager
  logAPICall' request'
  return $ tokensFromResponse (responseBody response)

oauthSignRequestWithVerifier :: (?appConfig :: AppConfig)
                             => OAuthVerifier
                             -> OAuthToken
                             -> Request
                             -> IO Request
oauthSignRequestWithVerifier verifier tempToken = signOAuth oauthApp credsWithVerifier
  where
    credentials       = newCredential (token tempToken)
                                      (tokenSecret tempToken)
    credsWithVerifier = injectVerifier (unOAuthVerifier verifier) credentials
    oauthApp          = newOAuth { oauthConsumerKey    = consumerToken ?appConfig
                                 , oauthConsumerSecret = consumerSecret ?appConfig }

oauthSignRequest :: (?appConfig :: AppConfig)
                 => OAuthToken
                 -> Request
                 -> IO Request
oauthSignRequest tok req = signOAuth oauthApp credentials req
  where
    credentials = newCredential (token tok)
                                (tokenSecret tok)
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?appConfig
                           , oauthConsumerSecret = consumerSecret ?appConfig }

oauthSignRequestWithEmptyCredentials :: (?appConfig :: AppConfig)
                                     => Request
                                     -> IO Request
oauthSignRequestWithEmptyCredentials = signOAuth oauthApp credentials
  where
    credentials = emptyCredential
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?appConfig
                           , oauthConsumerSecret = consumerSecret ?appConfig }

disconnectURL :: String
disconnectURL = "https://appcenter.intuit.com/api/v1/connection/disconnect"

accessTokenURL :: String
accessTokenURL = "https://oauth.intuit.com/oauth/v1/get_access_token"

temporaryTokenURL :: String
temporaryTokenURL = "https://oauth.intuit.com/oauth/v1/get_request_token"

authorizationURL :: ByteString
authorizationURL = "https://appcenter.intuit.com/Connect/Begin"

authorizationURLForToken :: OAuthToken -> ByteString 
authorizationURLForToken oatoken = authorizationURL <> "?oauth_token=" <> (token oatoken)
 
handleQuickBooksTokenResponse :: String -> Maybe OAuthToken -> Either String (QuickBooksResponse OAuthToken)
handleQuickBooksTokenResponse _ (Just tokensInResponse) = Right $ QuickBooksAuthResponse tokensInResponse
handleQuickBooksTokenResponse errorMessage Nothing      = Left errorMessage

tokensFromResponse :: BSL.ByteString -> Maybe OAuthToken
tokensFromResponse response = OAuthToken `liftM` lookup "oauth_token" responseParams
                                         `ap` lookup "oauth_token_secret" responseParams
  where responseParams = parseSimpleQuery (BSL.toStrict response)
