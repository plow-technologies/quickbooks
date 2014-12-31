{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}

module QuickBooks.Authentication
  ( getTempOAuthCredentialsRequest
  , getAccessTokensRequest
  , oauthSignRequest
  ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import QuickBooks.Types
import Network.HTTP.Client (Manager, Request(..), RequestBody(RequestBodyLBS), responseBody, parseUrl, httpLbs)
import Network.HTTP.Types.URI
import Web.Authenticate.OAuth (signOAuth, newCredential, emptyCredential, newOAuth, OAuth(..))

import QuickBooks.Logging (logAPICall, Logger)

getTempOAuthCredentialsRequest :: ( ?apiConfig :: APIConfig
                                  , ?manager   :: Manager
                                  , ?logger    :: Logger                
                                  ) => CallbackURL
                                    -> IO (Either String (QuickBooksResponse OAuthToken)) -- ^ An OAuthToken with temporary credentials
getTempOAuthCredentialsRequest callbackURL = do
  request  <- parseUrl $ concat [temporaryTokenURL, "?oauth_callback=", callbackURL]
  request' <- oauthSignRequestWithEmptyCredentials request {method="POST", requestBody = RequestBodyLBS ""}
  logAPICall request'
  response <- httpLbs request' ?manager
  case tempTokensFromResponse (responseBody response) of
    Just tempTokens -> return $ Right $ QuickBooksAuthResponse tempTokens
    Nothing         -> return $ Left ("Couldn't find tokens in QuickBooks response" :: String)

getAccessTokensRequest :: ( ?apiConfig :: APIConfig
                          , ?manager   :: Manager
                          ) => OAuthVerifier                                      -- ^ The OAuthVerifier provided to the callback
                            -> IO (Either String (QuickBooksResponse OAuthToken)) -- ^ OAuthToken containing access tokens
                                                                                  --   for a resource-owner
getAccessTokensRequest = undefined


oauthSignRequest :: (?apiConfig :: APIConfig) => Request -> IO Request
oauthSignRequest = signOAuth oauthApp credentials
  where
    credentials = newCredential (oauthToken ?apiConfig)
                                (oauthSecret ?apiConfig)
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                           , oauthConsumerSecret = consumerSecret ?apiConfig }

oauthSignRequestWithEmptyCredentials :: (?apiConfig :: APIConfig) => Request -> IO Request
oauthSignRequestWithEmptyCredentials = signOAuth oauthApp credentials
  where
    credentials = emptyCredential
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                           , oauthConsumerSecret = consumerSecret ?apiConfig }

temporaryTokenURL :: String
temporaryTokenURL = "https://oauth.intuit.com/oauth/v1/get_request_token"

tempTokensFromResponse :: BSL.ByteString -> Maybe OAuthToken
tempTokensFromResponse response = OAuthToken <$> lookup "oauth_token" responseParams
                                             <*> lookup "oauth_token_secret" responseParams
  where responseParams = parseSimpleQuery (BSL.toStrict response)
