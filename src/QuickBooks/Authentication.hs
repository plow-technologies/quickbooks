{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE OverloadedStrings  #-}

module QuickBooks.Authentication
  ( acquireTempTokens
  , acquireAccessTokens
  , oauthSignRequest
  ) where

import QuickBooks.Types (APIConfig(..), OAuthVerifier(..))
import Network.HTTP.Client (Manager, Request)
import Web.Authenticate.OAuth (signOAuth, newCredential, emptyCredential, newOAuth, OAuth(..))

acquireTempTokens :: ( ?apiConfig :: APIConfig
                     , ?manager   :: Manager
                     ) => IO APIConfig -- ^ An APIConfig with temporary credentials
acquireTempTokens = do
  let apiConfig = ?apiConfig
  request <- parseUrl

acquireAccessTokens :: ( ?apiConfig :: APIConfig
                       , ?manager   :: Manager
                       ) => OAuthVerifier -- ^ The OAuthVerifier provided to the callback
                         -> IO APIConfig     -- ^ An APIConfig containing access tokens
                                          --   for a resource-owner
acquireAccessTokens = undefined


oauthSignRequestWithEmptyCredentials :: (?apiConfig :: APIConfig) => Request -> IO Request
oauthSignRequestWithEmptyCredentials = signOAuth oauthApp credentials
  where
    credentials = emptyCredential
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                           , oauthConsumerSecret = consumerSecret ?apiConfig }

oauthSignRequest :: (?apiConfig :: APIConfig) => Request -> IO Request
oauthSignRequest = signOAuth oauthApp credentials
  where
    credentials = newCredential (oauthToken ?apiConfig)
                                (oauthSecret ?apiConfig)
    oauthApp    = newOAuth { oauthConsumerKey    = consumerToken ?apiConfig
                           , oauthConsumerSecret = consumerSecret ?apiConfig }
