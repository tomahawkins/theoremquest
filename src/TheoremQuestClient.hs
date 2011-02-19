module Main (main) where

import Data.Maybe
import Network.HTTP
import Network.URI
import System.Environment
import Text.JSON

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
        Right r -> case decode $ rspBody r of
          Error e -> error e
          Ok a -> print (a :: Rsp)
  where
  formatReq a = Request
    { rqURI = fromJust $ parseURI "http://localhost:8000"
    , rqMethod = POST
    , rqHeaders = headers
    , rqBody = body
    }
    where
    (headers, body) = formatJSON a

request :: [String] -> Maybe Req
request args = case args of
  ["newuser", name, email] -> Just $ NewUser name email
  ["ping"] -> Just Ping
  _ -> Nothing

