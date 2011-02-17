module Main (main) where

import Data.IORef
import Network.HTTP.Server
import Network.Socket
import Network.URL
import System.Environment
import Text.JSON

import LoK
import Transactions

help :: IO ()
help = putStrLn "usage: themoremquest-server [-p <port-number>] [-r <restore-file>] <log-file>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> help
    args -> runLoK (restore args) (last args) $ lowerIO $ \ lib -> server $ handler lib
  where
  restore :: [String] -> Maybe FilePath
  restore [] = Nothing
  restore ("-r" : file : _) = Just file
  restore (_ : rest) = restore rest

  --port :: [String] -> PortNumber

handler :: IORef Library -> SockAddr -> URL -> Request String -> IO (Response String)
handler lib _ _ req = case decode $ rqBody req of
    Error e -> do
      putStrLn $ "request parse error: " ++ e
      send BadRequest UnknownReq
    Ok req -> do
      putStrLn $ "request: " ++ show req
      rsp <- liftLoK lib $ transact req
      send OK rsp
  where
  send :: StatusCode -> Rsp -> IO (Response String)
  send status rsp = return (respond status :: Response String) { rspHeaders = headers, rspBody = body } where (headers, body) = formatJSON rsp

