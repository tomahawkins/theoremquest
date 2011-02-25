module Main (main) where

import Data.Maybe
import Network.HTTP
import Network.URI

import TheoremQuest

main :: IO ()
main = do
  a <- getContents
  putStrLn "Content-Type: text/plain\n"
  case lines a of
    [req] -> transact req
    _ -> putStrLn "invalid request"

-- | Conduct a transaction with the server.
transact :: String -> IO ()
transact req = do
  r <- simpleHTTP $ formatReq req
  case r of
    Left e -> putStrLn $ "failed transaction with server: " ++ show e
    Right r -> putStr $ rspBody r
  where
  formatReq a = Request
    { rqURI = fromJust $ parseURI "http://localhost:8000"
    , rqMethod = POST
    , rqHeaders = headers
    , rqBody = body
    }
    where
    (headers, body) = formatText a
