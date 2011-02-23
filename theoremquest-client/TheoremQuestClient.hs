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
  , "  tq -- a simple TheoremQuest client"
  , ""
  , "SYNOPSYS"
  , "  tq COMMAND { ARGUMENT }"
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
  , "  tq infer <inference-rule>"
  , "    Apply an inference rule to create a new theorem."
  , ""
  , "  tq theorem <theorem-id>"
  , "    Print the assumptions and proposition of a theorem."
  , ""
  , "  tq check-term <term>"
  , "    Check the syntax (parsing) of a term."
  , ""
  , "  tq check-variable <variable>"
  , "    Check the syntax (parsing) of a variable."
  , ""
  , "  tq check-inference <inference>"
  , "    Check the syntax (parsing) of an inference rule."
  , ""
  , "ENVIRONMENT VARIABLES"
  , "  THEOREMQUEST_USER"
  , "    The TheoremQuest username to use for transactions.  Required for inference commands."
  , ""
  ]

main :: IO ()
main = getArgs >>= go

-- | Conduct a transaction with the server.
transact :: Req -> IO Rsp
transact req = do
  r <- simpleHTTP $ formatReq req
  case r of
    Left e -> error $ "failed transaction: " ++ show e
    Right r -> case maybeRead $ rspBody r of
      Just a -> return a
      Nothing -> error $ "response parse error: " ++ rspBody r
  where
  formatReq a = Request
    { rqURI = fromJust $ parseURI "http://localhost:8000"
    , rqMethod = POST
    , rqHeaders = headers
    , rqBody = body
    }
    where
    (headers, body) = formatHaskell a

go :: [String] -> IO ()
go args = case args of
  ["newuser", name, email] -> transact (NewUser name email) >>= print
  ["ping"] -> transact Ping >>= print
  ["theorem", n] -> do
    r1 <- transact (TheoremAssumptions theorem)
    r2 <- transact (TheoremConclusion  theorem)
    case (r1, r2) of
      (Terms a, Term b) -> do
        putStrLn "assumptions:"
        mapM_ print a
        putStrLn "conclusion:"
        print b
      _ -> print (r1, r2)
    where
    theorem = read n
  ["infer", a] -> do
    user <- username
    putStrLn a
    r <- transact $ Inference user $ read a
    print r
  ["check-term", a] -> print (read a :: Term)
  ["check-variable", a] -> print (read a :: Variable)
  ["check-inference", a] -> print (read a :: Inference TheoremId)
  _ -> help

username :: IO String
username = do
  env <- getEnvironment
  case lookup "THEOREMQUEST_USER" env of
    Just user -> return user
    Nothing -> error "environment variable THEOREMQUEST_USER not set"
