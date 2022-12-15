module Util.HTTP (getURL) where

import Data.ByteString (ByteString)
import Network.HTTP.Simple (Request, getResponseBody, httpBS)

getURL :: Request -> IO ByteString
getURL url = getResponseBody <$> httpBS url
