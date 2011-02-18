module Library
  (
  -- * Types
    Library
  -- * Library Init
  , initLibrary
  -- * Library Transaction
  , transact
  ) where

import Control.Monad
import Text.JSON

import Logic
import Transactions

data Library = Library
  { libNextTheoremId :: TheoremId
  , libLogFile       :: FilePath
  , libTheorems      :: [(TheoremId, Theorem)]
  , libUsers         :: [User]
  , libUserTheorems  :: [(User, TheoremId)]
  , libDiscoveries   :: [(User, TheoremId)]
  , libAnnotations   :: [(TheoremId, String)]
  }

-- | Initialized library.
initLibrary :: Maybe FilePath -> FilePath -> IO Library
initLibrary oldLog newLog = do
  writeFile newLog ""
  case oldLog of
    Nothing -> return lib
    Just oldLog -> readFile oldLog >>= foldM restore lib . lines
  where
  lib = Library
    { libNextTheoremId = 0
    , libLogFile       = newLog
    , libTheorems      = []
    , libUsers         = []
    , libUserTheorems  = []
    , libDiscoveries   = []
    , libAnnotations   = []
    }
  restore :: Library -> String -> IO Library
  restore lib req = case decode req of
    Error msg -> do
      putStrLn $ "restore req parse error: " ++ msg ++ " : " ++ req
      return lib
    Ok a -> transact lib a >>= return . snd

-- | Conduct a 'Req' to 'Rsp' transaction.
transact :: Library -> Req -> IO (Rsp, Library)
transact lib req = do
  appendFile (libLogFile lib) $ encode req ++ "\n"
  return (UnknownReq, lib)

