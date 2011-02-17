module Main (main) where

import Data.Maybe
import Network.HTTP
import Network.URI
import Text.JSON

import Transactions

main :: IO ()
main = do
  r <- simpleHTTP req
  case r of
    Left e -> print e
    Right r -> case decode $ rspBody r of
      Error e -> error e
      Ok a -> print (a :: Rsp)
  where
  req = Request
    { rqURI = fromJust $ parseURI "http://localhost:8000"
    , rqMethod = POST
    , rqHeaders = headers
    , rqBody = body
    }
  (headers, body) = formatJSON Req

