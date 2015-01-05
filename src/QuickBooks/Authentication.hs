{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

module QuickBooks.Authentication
  ( getTempOAuthCredentialsRequest
  , getAccessTokensRequest
  , oauthSignRequest
  ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (unpack)
import QuickBooks.Types
import Network.HTTP.Client (Manager, Request(..), RequestBody(RequestBodyLBS), responseBody, parseUrl, httpLbs)
import Network.HTTP.Types.URI
import Web.Authenticate.OAuth (signOAuth, newCredential, emptyCredential, injectVerifier, newOAuth, OAuth(..))

import QuickBooks.Logging (logAPICall, Logger)

getTempOAuthCredentialsRequest :: ( ?apiConfig :: APIConfig
                                  , ?manager   :: Manager
                                  , ?logger    :: Logger                
                                  ) => CallbackURL
                                    -> IO (Either String (QuickBooksResponse OAuthToken)) -- ^ An OAuthToken with temporary credentials

getTempOAuthCredentialsRequest callbackURL =
  return.handleQuickBooksTokenResponse "Couldn't get temporary tokens" =<< tokensRequest
    where tokensRequest = getTokens temporaryTokenURL "?oauth_callback=" callbackURL oauthSignRequestWithEmptyCredentials

getAccessTokensRequest :: ( ?apiConfig :: APIConfig
                          , ?manager   :: Manager
                          , ?logger    :: Logger  
                          ) => OAuthVerifier                                      -- ^ The OAuthVerifier provided to the callback
                            -> OAuthToken                                         -- ^ The previously-acquired temp tokens to
                                                                                  --   use in signing this request
                            -> IO (Either String (QuickBooksResponse OAuthToken)) -- ^ OAuthToken containing access tokens
                                                                                  --   for a resource-owner
getAccessTokensRequest verifier tempToken =
  return.handleQuickBooksTokenResponse "Couldn't get access tokens" =<< tokensRequest
  where
    tokensRequest = getTokens accessTokenURL "?oauth_token=" (unpack $ token tempToken)
                                                             (oauthSignRequestWithVerifier verifier tempToken)


getTokens :: ( ?apiConfig :: APIConfig
             , ?manager   :: Manager
             , ?logger    :: Logger 
             ) => String                  -- ^ Endpoint to request the token
               -> String                  -- ^ URL parameter name
               -> String                  -- ^ URL parameter value
               -> ((?apiConfig :: APIConfig) => Request -> IO Request) -- ^ Signing function
               -> IO (Maybe OAuthToken)
getTokens tokenURL parameterName parameterValue signRequest = do
  request  <- parseUrl $ concat [tokenURL, parameterName, parameterValue]
  request' <- signRequest request { method="POST", requestBody = RequestBodyLBS "" }
  response <- httpLbs request' ?manager
  logAPICall request'
  return $ tokensFromResponse (responseBody response)


oauthSignRequestWithVerifier :: (?apiConfig :: APIConfig) => OAuthVerifier -> OAuthToken -> Request -> IO Request
oauthSignRequestWithVerifier verifier tempTokens = signOAuth oauthApp credsWithVerifier
  where
    credentials       = newCredential (token tempTokens)
                                      (tokenSecret tempTokens)
    credsWithVerifier = injectVerifier (unOAuthVerifier verifier) credentials
    oauthApp          = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                                 , oauthConsumerSecret = consumerSecret ?apiConfig }


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

accessTokenURL :: String
accessTokenURL = "https://oauth.intuit.com/oauth/v1/get_access_token"

temporaryTokenURL :: String
temporaryTokenURL = "https://oauth.intuit.com/oauth/v1/get_request_token"

handleQuickBooksTokenResponse :: String -> Maybe OAuthToken -> Either String (QuickBooksResponse OAuthToken)
handleQuickBooksTokenResponse _ (Just tokensInResponse) = Right $ QuickBooksAuthResponse tokensInResponse
handleQuickBooksTokenResponse errorMessage Nothing      = Left errorMessage

tokensFromResponse :: BSL.ByteString -> Maybe OAuthToken
tokensFromResponse response = OAuthToken <$> lookup "oauth_token" responseParams
                                         <*> lookup "oauth_token_secret" responseParams
  where responseParams = parseSimpleQuery (BSL.toStrict response)
