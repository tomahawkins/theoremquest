module TheoremQuest.Logic
  ( Term          (..)
  , Proposition
  , Variable      (..)
  , Theorem
  , TypeVariable  (..)
  , TypeTerm      (..)
  , Inference     (..)
  , Axiom         (..)
  , (=.)
  , assumptions
  , conclusion
  , inference
  ) where

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

data Theorem = Theorem [Proposition] Proposition

-- | Assumptions of a 'Theorem'.
assumptions :: Theorem -> [Proposition]
assumptions (Theorem a _) = a

-- | Conclusion of a 'Theorem'.
conclusion :: Theorem -> Proposition
conclusion (Theorem _ a) = a

data TypeVariable
  = TypeVariable
  deriving (Show, Read, Eq)

data TypeTerm
  = TypeTerm
  deriving (Show, Read, Eq)

data Inference a
  = REFL                Term
  | TRANS               a a
  | MK_COMB             a a
  | ABS                 Term a
  | BETA                Term
  | ASSUME              Term
  | EQ_MP               a a
  | DEDUCT_ANTISYM_RULE a a
  | INST                a [(Variable, Term)]
  | INST_TYPE           a [(TypeVariable, TypeTerm)]
  | AXIOM               Axiom
  deriving (Show, Read)

data Axiom
  = Axiom
  deriving (Show, Read)

(=.) :: Term -> Term -> Term
a =. b = Comb (Comb (Const "=" TypeTerm {-"a -> a -> bool"-}) a) b

-- | Creates a 'Theorem' from an 'Inference' rule application.
inference :: Inference Theorem -> Maybe Theorem
inference rule = case rule of
  REFL a -> validateTheorem [] (a =. a)
  TRANS (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) (Theorem b1 (Comb (Comb (Const "=" _) b2) b3)) | a3 == b2 -> validateTheorem (union a1 b1) (a2 =. b3)
  MK_COMB (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) (Theorem b1 (Comb (Comb (Const "=" _) b2) b3)) -> validateTheorem (union a1 b1) (Comb a2 b2 =. Comb a3 b3)
  ABS (Var x) (Theorem a1 (Comb (Comb (Const "=" _) a2) a3)) | not $ any (freeIn x) a1 -> validateTheorem a1 (Abs x a2 =. Abs x a3)
  BETA a@(Comb (Abs x1 t)(Var x2)) | x1 == x2 -> validateTheorem [] $ a =. t
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
  _ -> Nothing

-- | Validates (type checks) a theorem.
validateTheorem :: [Term] -> Term -> Maybe Theorem
validateTheorem assumptions proposition = Just $ Theorem assumptions proposition --XXX

-- | Checks if a variable is free in a term.
freeIn :: Variable -> Term -> Bool
freeIn = undefined

