{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module QuickBooks.Logging ( logAPICall
                          , logAPICall'
                          , Logger
                          , apiLogger
                          , getLogger
                          ) where

import Control.Monad             (when)
import Data.Char                 (toLower)
import Data.IORef
import Data.Monoid               ((<>))
import Data.String               (fromString)
import Data.String.Interpolate   (i)
import Data.Thyme
import Network.HTTP.Client       (Request(..),RequestBody(..),getUri)
import System.IO.Unsafe
import System.Locale 
import System.Log.FastLogger     (LogStr, pushLogStr, flushLogStr, newStdoutLoggerSet)
import qualified Data.ByteString.Char8 as BS

import QuickBooks.Types (APIConfig(..), Logger)

apiLogger :: IORef Logger
apiLogger = unsafePerformIO $ newIORef =<< newStdoutLoggerSet 0

getLogger :: IORef Logger -> IO Logger
getLogger = readIORef

logAPICall :: ( ?logger :: Logger
              , ?apiConfig :: APIConfig
              ) => Request -> IO ()
logAPICall req =
  let isLoggingEnabled = BS.map toLower (loggingEnabled ?apiConfig)
      in when (isLoggingEnabled == "true") $ logAPICall' req

logAPICall' :: (?logger :: Logger) => Request -> IO ()
logAPICall' req = do 
  now <- getCurrentTime
  let formattedTime = fromString $ formatTime defaultTimeLocale rfc822DateFormat now
  pushLogStr ?logger (requestLogLine req formattedTime)
  flushLogStr ?logger

requestLogLine :: Request -> LogStr -> LogStr
requestLogLine req formattedTime =
  let body = case requestBody req of
              (RequestBodyLBS bs) -> show bs 
              (RequestBodyBS bs)  -> show bs
              _                   -> ""
  in formattedTime <> fromString [i| [INFO] [#{method req}] [#{getUri req}] [#{body}]\n|]

