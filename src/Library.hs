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

import TheoremQuest

data Library = Library
  { libNextTheoremId :: TheoremId
  , libLogFile       :: FilePath
  , libTheorems      :: [(TheoremId, Theorem)]
  , libUsers         :: [(User, String)]
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
  restore lib req = transact lib (read req) >>= return . snd
  
-- | Conduct a 'Req' to 'Rsp' transaction.
transact :: Library -> Req -> IO (Rsp, Library)
transact lib req = do
  appendFile (libLogFile lib) $ show req ++ "\n"
  return $ case req of
    Ping -> (Ack, lib)
    NewUser user email
      | elem user $ fst $ unzip $ libUsers lib -> (Nack "username taken", lib)
      | otherwise -> (Ack, lib { libUsers = (user, email) : libUsers lib })

