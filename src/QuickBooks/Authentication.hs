{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module QuickBooks.Authentication 
  ( getTempOAuthCredentialsRequest
  , getAccessTokenRequest
  , oauthSignRequest
  , authorizationURLForToken
  , disconnectRequest
 ) where

import Control.Monad (void, liftM, ap)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (unpack, ByteString)

import Network.HTTP.Client (-- Manager
                            Request(..)
                             ,RequestBody(RequestBodyLBS)
                             ,responseBody
                             ,parseUrlThrow
                             ,setQueryString
                            --  ,queryString
                            -- ,getUri
                               ,httpLbs)

import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.URI               (escapeURIString
                                 , isUnescapedInURI)
--                                 , isUnescapedInURIComponent)
import qualified Network.HTTP.Client.TLS as TLS



import Web.Authenticate.OAuth (signOAuth
                              ,newCredential
                              ,emptyCredential
                              ,injectVerifier
                              ,newOAuth
                              ,OAuth(..))
                              


-- https://developer.intuit.com/docs/0100_quickbooks_online/0100_essentials/000500_authentication_and_authorization/implement_single_sign-on_with_openid#/Initiating_the_authentication_request


-- https://developer.intuit.com/docs/0100_quickbooks_online/0100_essentials/000500_authentication_and_authorization/implement_single_sign-on_with_openid#/Discovery_document

import QuickBooks.Logging (logAPICall')
import QuickBooks.Types
-- import Network.OAuth.OAuth2 (OAuth2 (..))
import qualified Network.OAuth.OAuth2 as OAuth2
-- import Data.Monoid ((<>))




-- _accessToken :: OAuth2.AccessToken



_Tryfetchaccesstoken :: IO ()
_Tryfetchaccesstoken  = do
  mgr <- TLS.getGlobalManager
  oauthTokenRslt <- OAuth2.authGetBS mgr accessToken "https://accounts.platform.intuit.com/v1/openid_connect/userinfo"
  case oauthTokenRslt of
    Left e       -> fail $ show e
    Right _  -> do
      print "wheee"
  where   _refreshToken = ""
          accessToken = OAuth2.AccessToken ("") Nothing Nothing Nothing Nothing
          _exchangeToken = ""
          _testOAuth :: OAuth2.OAuth2
          _testOAuth = OAuth2.OAuth2 {
             OAuth2.oauthClientId            = ""
           , OAuth2.oauthClientSecret        = "" 
           , OAuth2.oauthOAuthorizeEndpoint  = "https://appcenter.intuit.com/connect/oauth2" 
           , OAuth2.oauthAccessTokenEndpoint = "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer"
           , OAuth2.oauthCallback            =  Just "https://developer.intuit.com/v2/OAuth2Playground/RedirectUrl"
           }

_quickbooksAuthRequest :: IO ()
_quickbooksAuthRequest = do
            req <- parseUrlThrow "https://appcenter.intuit.com/connect/oauth2"
            mgr <- TLS.getGlobalManager
            let newReq = setQueryString parameters req 
--            return $ (getUri newReq)
            _ <- httpLbs newReq mgr
            return ()
  where
    parameters = [
       ("client_id"    , Just "" )
      ,("scope"        , Just ".intuit.quickbooks.accounting openid email profile")
      ,("redirect_uri" , Just "https://localhost/oauth2/callback")
      ,("response_type", Just "code")
      ,("state"        , Just "PlaygroundAuth")]

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
