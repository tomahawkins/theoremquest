module Transactions
  ( Req (..)
  , Rsp (..)
  , formatJSON
  ) where

import Codec.Binary.UTF8.String (encodeString)
import Network.HTTP
import Text.JSON

-- | Requests from client to server.
data Req
  = Req
  deriving Show

-- | Responses to client requests ('Req').
data Rsp
  = DeprecatedReq Rsp   -- ^ A warning to clients that the associated 'Req' will soon be obsolete.
  | UnknownReq          -- ^ Server did not recognize 'Req'.
  deriving Show

instance JSON Req where
  showJSON a = case a of
    Req -> JSArray [JSString $ toJSString "Req"]

  readJSON a = case a of
    JSArray [JSString a] | fromJSString a == "Req" -> Ok Req
    a -> Error $ "invalid Req: " ++ encode a

instance JSON Rsp where
  showJSON a = case a of
    DeprecatedReq a -> JSArray [JSString $ toJSString "DeprecatedReq", showJSON a]
    UnknownReq      -> JSArray [JSString $ toJSString "UnknownReq"]

  readJSON a = case a of
    JSArray [JSString a, b] | fromJSString a == "DeprecatedReq" -> case readJSON b of
      Ok b -> Ok $ DeprecatedReq b
      error -> error
    JSArray [JSString a] | fromJSString a == "UnknownReq" -> Ok UnknownReq
    a -> Error $ "invalid Req: " ++ encode a

formatJSON :: JSON a => a -> ([Header], String)
formatJSON a = ([Header HdrContentLength $ show $ length msg, Header HdrContentEncoding "UTF-8", Header HdrContentType "application/json"], msg)
  where
  msg = encodeString $ encode a

