module Main (main) where

import Data.IORef
import Network.HTTP.Server
import Network.Socket
import Network.URL
import System.Environment

import Library
import TheoremQuest

help :: IO ()
help = putStrLn "usage: tqd { -a <host-name>[:<port-number>] | -r <restore-file> } <log-file>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> help
    args -> do
      lib <- initLibrary (restore args) (last args) >>= newIORef
      serverWith (config args) (handler lib)
  where
  restore :: [String] -> Maybe FilePath
  restore [] = Nothing
  restore ("-r" : file : _ : _) = Just file
  restore (_ : rest) = restore rest

  config :: [String] -> Config
  config [] = defaultConfig
  config ("-a" : host : _ : _) = defaultConfig { srvHost = takeWhile (/= ':') host, srvPort = fromIntegral p }
    where
    p :: Int
    p | elem ':' host = read $ reverse $ takeWhile (/= ':') $ reverse host
      | otherwise = 8000
  config (_ : rest) = config rest

handler :: IORef Library -> SockAddr -> URL -> Request String -> IO (Response String)
handler lib _ _ req = case maybeRead $ rqBody req of
  Nothing -> do
    putStrLn $ "request parse error: " ++ rqBody req
    sendRsp BadRequest UnknownReq
  Just req -> do
    putStrLn $ "request: " ++ show req
    l <- readIORef lib
    (rsp, l) <- transact l req
    writeIORef lib l
    send req OK rsp
  where
  send :: Req -> StatusCode -> Rsp -> IO (Response String)
  send req = case req of
    RspInJSON _ -> sendRspInJSON
    _           -> sendRsp
    
  sendRspInJSON :: StatusCode -> Rsp -> IO (Response String)
  sendRspInJSON status rsp = return (respond status :: Response String) { rspHeaders = headers, rspBody = body } where (headers, body) = formatJSON rsp

  sendRsp :: StatusCode -> Rsp -> IO (Response String)
  sendRsp status rsp = return (respond status :: Response String) { rspHeaders = headers, rspBody = body } where (headers, body) = formatHaskell rsp

