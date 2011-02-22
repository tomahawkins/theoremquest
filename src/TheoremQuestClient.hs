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
  , "  tq -- a simple shell-based theoremquest client"
  , ""
  , "SYNOPSYS"
  , "  tq [ command { option } { arguments } ]"
  , ""
  , "COMMANDS"
  , "  tq help"
  , "    Print this information."
  , ""
  , "  tq ping"
  , "    Ping the server."
  , ""
  , "  tq newuser <username> <email>"
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
    (headers, body) = formatHaskell a

request :: [String] -> Maybe Req
request args = case args of
  "--json" : rest -> request rest >>= return . RspInJSON
  ["newuser", name, email] -> Just $ NewUser name email
  ["ping"] -> Just Ping
  _ -> Nothing

{-
login :: IO ()
login = do
  putStrLn "Welcome to TheoremQuest"
  putStr "username: "
  hFlush stdout
  user <- getLine
  shell user

shell :: String -> IO ()
shell user = do
  putStr "theoremquest> "
  hFlush stdout
  l <- getLine
  case l of
    "?" -> shellHelp
    "help" -> shellHelp
    "exit" -> return ()
    "" -> return ()
    a -> putStrLn ("unknown command: " ++ a ++ ", type ? for help")
  when (l /= "exit") (shell user)

shellHelp :: IO ()
shellHelp = putStrLn "help"
-}
