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

type User = String
type Email = String

data Library = Library
  { libNextId             :: Int
  , libLogFile            :: FilePath
  , libTheorems           :: [(TheoremId, Theorem)]
  , libTerms              :: [(TermId, Term)]
  , libUsers              :: [(User, Email)]
  , libTheoremsDiscovered :: [(User, TheoremId)]
  , libTheoremsControlled :: [(User, TheoremId)]
  , libAnnotations        :: [(TheoremId, String)]
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
    { libNextId             = 0
    , libLogFile            = newLog
    , libTheorems           = []
    , libTerms              = []
    , libUsers              = []
    , libTheoremsDiscovered = []
    , libTheoremsControlled = []
    , libAnnotations        = []
    }
  restore :: Library -> String -> IO Library
  restore lib req = transact lib (read req) >>= return . snd
  
-- | Conduct a 'Req' to 'Rsp' transaction.
transact :: Library -> Req -> IO (Rsp, Library)
transact lib req = do
  appendFile (libLogFile lib) $ show req ++ "\n"
  return $ transact' lib req

transact' :: Library -> Req -> (Rsp, Library)
transact' lib req = case req of
  Ping -> (Ack, lib)
  NewUser user email
    | elem user $ fst $ unzip $ libUsers lib -> (Nack "username taken", lib)
    | otherwise -> (Ack, lib { libUsers = (user, email) : libUsers lib })
  RspInJSON req -> transact' lib req


