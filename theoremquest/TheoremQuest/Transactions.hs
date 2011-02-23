module TheoremQuest.Transactions
  ( Req (..)
  , Rsp (..)
  , User
  , Email
  , TheoremId
  , formatJSON
  , formatText
  , formatHaskell
  , maybeRead
  ) where

import Codec.Binary.UTF8.String (encodeString)
import Network.HTTP
import Text.JSON

import TheoremQuest.Logic

type User = String
type Email = String
type TheoremId = Int

-- | Requests from client to server.
data Req
  = Ping                                   -- ^ Ping server.
  | NewUser User Email                     -- ^ New user: username, email.
  | RspInJSON Req                          -- ^ Send response in JSON.
  | Inference User (Inference TheoremId)   -- ^ Submit an inference.  Server will validate the inference and return a theorem.
  | TheoremAssumptions TheoremId           -- ^ Request a theorem's assumptions.
  | TheoremConclusion  TheoremId           -- ^ Request a theorem's conclusion.
  | TheoremSearch Term Int                 -- ^ Search for a theorem similar to a term.  Return a list of ids starting at the given index.
  deriving (Show, Read)

-- | Responses to client requests.
data Rsp
  = DeprecatedReq Rsp   -- ^ A warning to clients that the associated 'Req' will soon be obsolete.
  | UnknownReq          -- ^ Server did not recognize 'Req'.
  | Ack                 -- ^ Acknowledge.
  | Nack String         -- ^ No acknowledge with reason.
  | Id Int              -- ^ A unique id.  Usually a 'TheoremId'.
  | Ids [Int]           -- ^ A list of unique ids.
  | Term Term           -- ^ A term.
  | Terms [Term]        -- ^ A list of terms.
  deriving (Show, Read)

instance JSON Rsp where
  readJSON = undefined
  showJSON = undefined
  {-
  showJSON a = case a of
    DeprecatedReq a -> JSArray [JSString $ toJSString "DeprecatedReq", showJSON a]
    UnknownReq      -> JSArray [JSString $ toJSString "UnknownReq"]
    Ack             -> JSArray [JSString $ toJSString "Ack"]
    Nack a          -> JSArray $ map (JSString . toJSString) ["Nack", a]
    -}

instance JSON Term where
  readJSON = undefined
  showJSON = undefined

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

-- | HTTP headers and body for shown Haskell type transfer.
formatHaskell :: Show a => a -> ([Header], String)
formatHaskell = formatText . show

-- | Maybe read, on parse errors return Nothing.
maybeRead :: Read a => String -> Maybe a
maybeRead s = case [x | (x,t) <- reads s, ("","") <- lex t] of
  [x] -> Just x
  _ -> Nothing
