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
  , "    The TheoremQuest username to use for transactions.  Required for infer commands."
  , ""
  , "  THEOREMQUEST_SERVER"
  , "    URI of the TheoremQuest server.  Default: http://theoremquest.org/cgi-bin/tqcgi"
  , ""
  ]

main :: IO ()
main = getArgs >>= go

-- | Conduct a transaction with the server.
transact :: Req -> IO Rsp
transact req = do
  server <- server
  r <- simpleHTTP $ formatReq server req
  case r of
    Left e -> error $ "failed transaction: " ++ show e
    Right r -> case maybeRead $ rspBody r of
      Just a -> return a
      Nothing -> error $ "response parse error: " ++ show (rspHeaders r) ++ "  " ++ rspBody r
  where
  formatReq uri a = Request
    { rqURI = uri
    , rqMethod = POST
    , rqHeaders = headers
    , rqBody = body
    }
    where
    (headers, body) = formatHaskell a

username :: IO String
username = do
  env <- getEnvironment
  case lookup "THEOREMQUEST_USER" env of
    Just user -> return user
    Nothing -> error "environment variable THEOREMQUEST_USER not set"

server :: IO URI
server = do
  env <- getEnvironment
  return $ fromJust $ parseURI $ case lookup "THEOREMQUEST_SERVER" env of
    Nothing -> "http://theoremquest.org/cgi-bin/tqcgi"
    Just a  -> a

go :: [String] -> IO ()
go args = case args of
  ["newuser", name, email] -> transact (NewUser name email) >>= print
  ["ping"] -> transact Ping >>= print
  ["theorem", n] -> do
    r1 <- transact (TheoremAssumptions theorem)
    case r1 of
      Terms a -> do
        r2 <- transact (TheoremConclusion theorem)
        case r2 of
          Term b -> do
            putStrLn "assumptions:"
            mapM_ print a
            putStrLn "conclusion:"
            print b
          _ -> print r2
      _ -> print r1
    where
    theorem = read n
  ["infer", a] -> do
    user <- username
    r <- transact $ Inference user $ read a
    print r
  ["check-term", a] -> print (read a :: Term)
  ["check-variable", a] -> print (read a :: Variable)
  ["check-inference", a] -> print (read a :: Inference TheoremId)
  _ -> help

