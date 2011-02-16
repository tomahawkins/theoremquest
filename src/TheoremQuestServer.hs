module Main (main) where

import Codec.Binary.UTF8.String
import Network.HTTP.Server
import Network.Socket
import Network.URL
import Text.JSON

import LoK ()

main :: IO ()
main = server handler

handler :: SockAddr -> URL -> Request String -> IO (Response String)
handler _ _ _ = return $ sendJSON OK $ JSObject $ toJSObject [("hi", JSString $ toJSString "there")]

-- sendText OK "Hi Dude!"

sendText :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentType "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

sendJSON :: StatusCode -> JSValue -> Response String
sendJSON s v    = replaceHeader HdrContentType "application/json"
                $ sendText s (showJSValue v "")

{-
sendHTML :: StatusCode -> Html -> Response String
sendHTML s v    = replaceHeader HdrContentType "text/html"
                $ sendText s (renderHtml v)
-}

sendScript     :: StatusCode -> String -> Response String
sendScript s v  = replaceHeader HdrContentType "application/x-javascript"
                $ sendText s v

