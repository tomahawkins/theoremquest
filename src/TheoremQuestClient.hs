module Main (main) where

import Data.Maybe
import Network.HTTP
import Network.URI
import System.Environment

import TheoremQuest

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  theoremquest -- a simple shell interface to theoremquest"
  , ""
  , "SYNOPSYS"
  , "  theoremquest command { option } { arguments }"
  , ""
  , "COMMANDS"
  , "  theoremquest help"
  , "    Print this information."
  , ""
  , "  theoremquest ping"
  , "    Ping the server."
  , ""
  , "  theoremquest newuser <username> <email>"
  , "    Create a new user."
  , ""
  ]

main :: IO ()
main = do
  args <- getArgs
  case request args of
    Nothing -> help
    Just req -> do
      r <- simpleHTTP $ formatReq req
      case r of
        Left e -> print e
        Right r -> case maybeRead $ rspBody r :: Maybe Rsp of
          Just a -> print a
          Nothing -> putStrLn $ "response parse error: " ++ rspBody r
  where
  formatReq a = Request
    { rqURI = fromJust $ parseURI "http://localhost:8000"
    , rqMethod = POST
    , rqHeaders = headers
    , rqBody = body
    }
    where
    (headers, body) = formatText $ show a

request :: [String] -> Maybe Req
request args = case args of
  "--json" : rest -> request rest >>= return . RspInJSON
  ["newuser", name, email] -> Just $ NewUser name email
  ["ping"] -> Just Ping
  _ -> Nothing

