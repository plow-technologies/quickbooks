{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module QuickBooks.Logging (logAPICall) where


import Data.Char                 (toLower)
import Data.String.Interpolate   (i)
import Network.HTTP.Client       ( Request(..)
                                 ,RequestBody(..)
                                 ,getUri)
import Data.String (fromString)
import System.Log.FastLogger (LoggerSet, LogStr, pushLogStr, flushLogStr)
import Data.Thyme
import System.Locale 
import Data.Monoid ((<>))

import qualified Data.ByteString.Char8 as BS
import QuickBooks.Types (APIConfig(..))

logAPICall :: ( ?logger :: LoggerSet
              , ?apiConfig :: APIConfig
              ) => Request -> IO ()
logAPICall req =
  let isLoggingEnabled = BS.map toLower (loggingEnabled ?apiConfig) in
  if isLoggingEnabled == "true"
    then logAPICall'
    else return ()
  where
    logAPICall' = do 
      now <- getCurrentTime
      let formattedTime = fromString $ formatTime defaultTimeLocale rfc822DateFormat now
      pushLogStr ?logger (requestLogLine req formattedTime)
      flushLogStr ?logger

-- Log line format - TimeStamp [Request Method] [Request URL] [Request Body]
requestLogLine :: Request -> LogStr -> LogStr
requestLogLine req formattedTime =
  let body = case requestBody req of
              (RequestBodyLBS bs) -> show bs 
              (RequestBodyBS bs)  -> show bs
              _                   -> ""
  in formattedTime <> fromString [i| [INFO] [#{method req}] [#{getUri req}] [#{body}]\n|]

