module LoK
  (
  -- * Types
    LoK
  , Term          (..)
  , Proposition
  , Variable      (..)
  , Theorem       (..)
  , TypeVariable  (..)
  , TypeTerm      (..)
  , Inference     (..)
  , UserInfo      (..)
  , TheoremInfo   (..)
  -- * Administrative
  , createUser
  -- * Derivation
  , inference
  , annotate
  -- * Queries
  , findTheorem
  , userInfo
  , theoremInfo
  , unproven
  -- * Run LoK Monad
  , runLoK
  ) where

import Control.Monad.IO.Class
import Data.List

infix 4 =.

data Term
  = Const String TypeTerm
  | Var   Variable
  | Abs   Variable Term
  | Comb  Term Term
  deriving (Show, Read, Eq)

-- | A boolean term.
type Proposition = Term

data Variable = Variable String TypeTerm
  deriving (Show, Read, Eq)

type TheoremId = Int

data Theorem = Theorem
  { theoremAssumptions :: [Proposition]
  , theoremProposition :: Proposition
  } deriving (Show, Read)

data TypeVariable
  = TypeVariable
  deriving (Show, Read, Eq)

data TypeTerm
  = TypeTerm
  deriving (Show, Read, Eq)

data Inference
  = REFL                Term
  | TRANS               Theorem Theorem
  | MK_COMB             Theorem Theorem
  | ABS                 Term Theorem
  | BETA                Term Term
  | ASSUME              Term
  | EQ_MP               Theorem Theorem
  | DEDUCT_ANTISYM_RULE Theorem Theorem
  | INST                Theorem [(Variable, Term)]
  | INST_TYPE           Theorem [(TypeVariable, TypeTerm)]
  | AXIOM               Axiom
  deriving (Show, Read)

data Axiom
  = Axiom
  deriving (Show, Read)

type User = String
data UserInfo = UserInfo
data TheoremInfo = TheoremInfo

data Command
  = CreateUser User
  | Inference User Inference
  | Annotate TheoremId String
  deriving (Show, Read)

data Library = Library
  { libNextTheoremId :: TheoremId
  , libLogFile       :: FilePath
  , libTheorems      :: [(TheoremId, Theorem)]
  , libUsers         :: [User]
  , libUserTheorems  :: [(User, TheoremId)]
  , libDiscoveries   :: [(User, TheoremId)]
  , libAnnotations   :: [(TheoremId, String)]
  }

data LoK a = LoK (Library -> IO (a, Library))

instance Monad LoK where
  return a = LoK $ \ lib -> return (a, lib)
  LoK f1 >>= f2 = LoK f3
    where
    f3 lib = do
      (a, lib) <- f1 lib
      let LoK f4 = f2 a
      f4 lib

instance MonadIO LoK where
  liftIO a = LoK $ \ lib -> do
    a <- a
    return (a, lib)

runLoK :: Maybe FilePath -> FilePath -> LoK () -> IO ()
runLoK oldLog newLog a = do
  writeFile newLog ""
  f Library
    { libNextTheoremId = 0
    , libLogFile       = newLog
    , libTheorems      = []
    , libUsers         = []
    , libUserTheorems  = []
    , libDiscoveries   = []
    , libAnnotations   = []
    }
  return ()
  where
  LoK f = restore >> a
  restore :: LoK ()
  restore = case oldLog of
    Nothing -> return ()
    Just oldLog -> do
      oldLog <- liftIO $ readFile oldLog
      mapM_ (command . read) $ lines oldLog
  command :: Command -> LoK ()
  command a = case a of
    CreateUser user -> createUser user >> return ()
    Inference user rule -> inference user rule >> return ()
    Annotate id msg -> annotate id msg

-- | Save command to log file.
logCommand :: Library -> Command -> IO ()
logCommand lib cmd = appendFile (libLogFile lib) (show cmd ++ "\n")

-- | Create a new account.  Returns True if success, False if user name taken.
createUser :: User -> LoK Bool
createUser user = LoK $ \ lib -> if elem user $ libUsers lib
  then return (False, lib)
  else logCommand lib (CreateUser user) >> return (True, lib { libUsers = user : libUsers lib })

(=.) :: Term -> Term -> Term
a =. b = Comb (Comb (Const "=" TypeTerm {-"a -> a -> bool"-}) a) b

-- | Applies an 'Inference' rule.  Returns either an error message or the resulting 'Theorem'.
inference :: User -> Inference -> LoK (Either String Theorem)
inference user rule = case rule of
  REFL a -> validateTheorem [] (a =. a)
  TRANS (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) (Theorem b1 (Comb (Comb (Const "=" _) b2) b3)) | a3 == b2 -> validateTheorem (union a1 b1) (a2 =. b3)
  MK_COMB (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) (Theorem b1 (Comb (Comb (Const "=" _) b2) b3)) -> validateTheorem (union a1 b1) (Comb a2 b2 =. Comb a3 b3)
  ABS (Var x) (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) | not $ any (freeIn x) a1 -> validateTheorem a1 (Abs x a2 =. Abs x a3)
  BETA (Var x) t -> validateTheorem [] $ Comb (Abs x t) (Var x) =. t
  ASSUME p -> validateTheorem [p] p
  EQ_MP (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) (Theorem b1 b2) | a2 == b2 -> validateTheorem (union a1 b1) a3
  DEDUCT_ANTISYM_RULE (Theorem a1 a2) (Theorem b1 b2) -> validateTheorem (union (delete b2 a1) (delete a2 b1)) $ a2 =. b2
  INST (Theorem assumes prop) subs -> validateTheorem (map (replace subs) assumes) (replace subs prop)
    where
    replace subs a = case a of
      Const _ _ -> a
      Var v -> case lookup v subs of
        Nothing -> a
	Just t  -> t
      Abs v a -> Abs v $ replace [ (v', t) | (v', t) <- subs, v /= v' ] a
      Comb a b -> Comb (replace subs a) (replace subs b)
  -- INST_TYPE           Theorem [(TypeVariable, TypeTerm)]
  -- AXIOM               Axiom
  _ -> return $ Left "invalid inference pattern"

  --logCommand lib $ Inference user rule
  -- XXX let theorem = Theorem (libNextTheoremId lib) (
  --return (Left "XXX", lib)
  where
  validateTheorem :: [Term] -> Term -> LoK (Either String Theorem)
  validateTheorem assumptions proposition = LoK $ \ lib -> return (Left "XXX", lib)

-- | Checks if a variable is free in a term.
freeIn :: Variable -> Term -> Bool
freeIn = undefined

-- | Marks a 'Theorem' with a note.
annotate :: TheoremId -> String -> LoK ()
annotate id msg = LoK $ \ lib -> if elem id [ id | (id, _) <- libTheorems lib ]
  then logCommand lib (Annotate id msg) >> return ((), lib { libAnnotations = (id, msg) : libAnnotations lib })
  else return ((), lib)

-- | Find a matching 'Theorem'.
findTheorem :: Proposition -> LoK (Maybe Theorem)
findTheorem = undefined

-- | User information.
userInfo :: User -> LoK UserInfo
userInfo = undefined

-- | Theorem information.
theoremInfo :: Theorem -> LoK TheoremInfo
theoremInfo = undefined

-- | Starting from an index, a list of 100 unproven propositions
-- grouped with theorems that need them.
-- Ordered by number of references.
unproven :: Int -> LoK [(Proposition, [Theorem])]
unproven = undefined


