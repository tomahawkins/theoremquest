module TheoremQuest.Transactions
  ( Req (..)
  , Rsp (..)
  , formatJSON
  , formatText
  , maybeRead
  ) where

import Codec.Binary.UTF8.String (encodeString)
import Network.HTTP
import Text.JSON

-- | Requests from client to server.
data Req
  = Ping
  | NewUser String String -- ^ username email
  | RspInJSON Req
  deriving (Show, Read)

-- | Responses to client requests ('Req').
data Rsp
  = DeprecatedReq Rsp   -- ^ A warning to clients that the associated 'Req' will soon be obsolete.
  | UnknownReq          -- ^ Server did not recognize 'Req'.
  | Ack                 -- ^ Acknowledge.
  | Nack String         -- ^ No acknowledge with reason.
  deriving (Show, Read)

instance JSON Rsp where
  showJSON a = case a of
    DeprecatedReq a -> JSArray [JSString $ toJSString "DeprecatedReq", showJSON a]
    UnknownReq      -> JSArray [JSString $ toJSString "UnknownReq"]
    Ack             -> JSArray [JSString $ toJSString "Ack"]
    Nack a          -> JSArray $ map (JSString . toJSString) ["Nack", a]

  readJSON = undefined

-- | HTTP headers and body for JSON transfer.
formatJSON :: JSON a => a -> ([Header], String)
formatJSON a = ([Header HdrContentLength $ show $ length msg, Header HdrContentEncoding "UTF-8", Header HdrContentType "application/json"], msg)
  where
  msg = encodeString $ encode a

-- | HTTP headers and body for text transfer.
formatText :: String -> ([Header], String)
formatText a = ([Header HdrContentLength $ show $ length msg, Header HdrContentEncoding "UTF-8", Header HdrContentType "text/plain"], msg)
  where
  msg = encodeString a

-- | Maybe read, on parse errors return Nothing.
maybeRead :: Read a => String -> Maybe a
maybeRead s = case [x | (x,t) <- reads s, ("","") <- lex t] of
  [x] -> Just x
  _ -> Nothing
