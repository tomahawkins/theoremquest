module Main (main) where

import Network.HTTP.Server
import Network.Socket
import Network.URL
import Text.JSON

import LoK ()
import Transactions

main :: IO ()
main = server handler

handler :: SockAddr -> URL -> Request String -> IO (Response String)
handler _ _ req = do
  case decode $ rqBody req of
    Error e -> putStrLn $ "request error: " ++ e
    Ok a -> putStrLn $ "request: " ++ show (a :: Req)
  return (respond OK :: Response String) { rspHeaders = headers, rspBody = body }
  where
  (headers, body) = formatJSON $ DeprecatedReq $ DeprecatedReq UnknownReq

