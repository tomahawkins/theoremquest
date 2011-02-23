module Library
  ( Library
  , initLibrary
  , transact
  ) where

import Control.Monad

import TheoremQuest

data Library = Library
  { libNextId             :: Int
  , libLogFile            :: FilePath
  , libTheorems           :: [(TheoremId, (Theorem, Inference Theorem))]
  , libUsers              :: [(User, Email)]
  , libTheoremsDiscovered :: [(User, TheoremId)]
  , libTheoremsControlled :: [(User, TheoremId)]
  , libAnnotations        :: [(Theorem, String)]
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
    , libUsers              = []
    , libTheoremsDiscovered = []
    , libTheoremsControlled = []
    , libAnnotations        = []
    }
  restore :: Library -> String -> IO Library
  restore lib req = transact lib (read req) >>= return . snd

-- | Next unique id.
nextId :: Library -> (Int, Library)
nextId lib = (libNextId lib, lib { libNextId = libNextId lib + 1 })
  
replaceTheoremIds :: Library -> Inference TheoremId -> Maybe (Inference Theorem)
replaceTheoremIds lib rule = case rule of
  REFL a -> Just $ REFL a
  TRANS a b -> do
    a <- theorem a
    b <- theorem b
    return $ TRANS a b
  MK_COMB a b -> do
    a <- theorem a
    b <- theorem b
    return $ MK_COMB a b
  ABS a b -> do
    b <- theorem b
    return $ ABS a b
  BETA a -> Just $ BETA a
  ASSUME a -> Just $ ASSUME a
  EQ_MP a b -> do
    a <- theorem a
    b <- theorem b
    return $ EQ_MP a b
  DEDUCT_ANTISYM_RULE a b -> do
    a <- theorem a
    b <- theorem b
    return $ DEDUCT_ANTISYM_RULE a b
  INST a b -> do
    a <- theorem a
    return $ INST a b
  INST_TYPE a b -> do
    a <- theorem a
    return $ INST_TYPE a b
  AXIOM a -> Just $ AXIOM a
  where
  theorem :: TheoremId -> Maybe Theorem
  theorem id = lookup id (libTheorems lib) >>= return . fst

-- | Conduct a 'Req' to 'Rsp' transaction.
transact :: Library -> Req -> IO (Rsp, Library)
transact lib req = do
  appendFile (libLogFile lib) $ show req ++ "\n"
  return $ response lib req

-- | Response to request.
response :: Library -> Req -> (Rsp, Library)
response lib req = case req of
  Ping -> (Ack, lib)

  NewUser user email
    | elem user $ fst $ unzip $ libUsers lib -> (Nack "username taken", lib)
    | otherwise -> (Ack, lib { libUsers = (user, email) : libUsers lib })

  RspInJSON req -> response lib req

  Inference user _ | not $ elem user [ user | (user, _) <- libUsers lib ] -> (Nack "unknown user", lib)
  Inference user a -> case replaceTheoremIds lib a of
    Nothing -> (Nack "unknown theorems in inference", lib)
    Just a -> case inference a of
      Nothing -> (Nack "invalid inference", lib)
      Just b -> (Id id, lib2)
        where
        (id, lib1) = nextId lib
        lib2 = lib1
          { libTheorems = (id, (b, a)) : libTheorems lib
          , libTheoremsDiscovered = (user, id) : libTheoremsDiscovered lib  --XXX  Need to check if theorem is unique, with variable renaming.
          , libTheoremsControlled = (user, id) : libTheoremsControlled lib  --XXX  Dito.
          }

  TheoremAssumptions a -> case lookup a $ libTheorems lib of
    Nothing -> (Nack "theorem not found", lib)
    Just (a, _) -> (Terms $ assumptions a, lib)

  TheoremConclusion a -> case lookup a $ libTheorems lib of
    Nothing -> (Nack "theorem not found", lib)
    Just (a, _) -> (Term $ conclusion a, lib)

  TheoremSearch _ _ -> (Ids [], lib)

