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
  = Ping
  | NewUser String String -- ^ username email
  deriving Show

-- | Responses to client requests ('Req').
data Rsp
  = DeprecatedReq Rsp   -- ^ A warning to clients that the associated 'Req' will soon be obsolete.
  | UnknownReq          -- ^ Server did not recognize 'Req'.
  | Ack                 -- ^ Acknowledge.
  | Nack String         -- ^ No acknowledge with reason.
  deriving Show

instance JSON Req where
  showJSON a = case a of
    Ping -> JSArray [JSString $ toJSString "Ping"]
    NewUser name email -> JSArray $ map (JSString . toJSString) ["NewUser", name, email]

  readJSON a = case a of
    JSArray [JSString req] | fromJSString req == "Ping" -> Ok Ping
    JSArray [JSString req, JSString username, JSString email] | fromJSString req == "NewUser" -> Ok $ NewUser (fromJSString username) (fromJSString email)
    a -> Error $ "invalid Req: " ++ encode a

instance JSON Rsp where
  showJSON a = case a of
    DeprecatedReq a -> JSArray [JSString $ toJSString "DeprecatedReq", showJSON a]
    UnknownReq      -> JSArray [JSString $ toJSString "UnknownReq"]
    Ack             -> JSArray [JSString $ toJSString "Ack"]
    Nack a          -> JSArray $ map (JSString . toJSString) ["Nack", a]

  readJSON a = case a of
    JSArray [JSString a, b] | fromJSString a == "DeprecatedReq" -> case readJSON b of
      Ok b -> Ok $ DeprecatedReq b
      error -> error
    JSArray [JSString a] | fromJSString a == "UnknownReq" -> Ok UnknownReq
    JSArray [JSString a] | fromJSString a == "Ack"        -> Ok Ack
    JSArray [JSString a, JSString b] | fromJSString a == "Nack" -> Ok $ Nack $ fromJSString b
    a -> Error $ "invalid Req: " ++ encode a

formatJSON :: JSON a => a -> ([Header], String)
formatJSON a = ([Header HdrContentLength $ show $ length msg, Header HdrContentEncoding "UTF-8", Header HdrContentType "application/json"], msg)
  where
  msg = encodeString $ encode a

