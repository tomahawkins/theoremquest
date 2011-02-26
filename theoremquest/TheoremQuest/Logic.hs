module TheoremQuest.Logic
  ( Term          (..)
  , Proposition
  , Variable      (..)
  , Theorem
  , Type          (..)
  , Inference     (..)
  , Axiom         (..)
  , TypeOf        (..)
  , (=.)
  , assumptions
  , conclusion
  , inference
  , wellTyped
  , freeIn
  , freeVariables
  ) where

import Data.List

infix  4 =.
infixr 1 :->

data Term
  = Const String Type
  | Var   Variable
  | Abs   Variable Term
  | Comb  Term Term
  deriving (Show, Read, Eq)

-- | A boolean term.
type Proposition = Term

data Variable = Variable String Type
  deriving (Show, Read)

instance Eq Variable where
  Variable a _ == Variable b _ = a == b

data Theorem = Theorem [Proposition] Proposition

-- | Assumptions of a 'Theorem'.
assumptions :: Theorem -> [Proposition]
assumptions (Theorem a _) = a

-- | Conclusion of a 'Theorem'.
conclusion :: Theorem -> Proposition
conclusion (Theorem _ a) = a

data Type
  = Bool
  | Type :-> Type
  | NotWellTyped
  deriving (Show, Read)

instance Eq Type where
  Bool == Bool = True
  (a :-> b) == (x :-> y) = a == x && b == y
  _ == _ = False


data Inference a
  = REFL           Term
  | TRANS          a a
  | MK_COMB        a a
  | ABS            Term a
  | BETA           Term
  | ASSUME         Term
  | EQ_MP          a a
  | DEDUCT_ANTISYM a a
  | INST           [(Variable, Term)] a
  | INST_TYPE      [(Type, Type)] a
  | AXIOM          Axiom
  deriving (Show, Read)

data Axiom
  = Axiom
  deriving (Show, Read)

class    TypeOf a        where typeOf :: a -> Type
instance TypeOf Type     where typeOf = id
instance TypeOf Variable where typeOf (Variable _ t) = t
instance TypeOf Term where
  typeOf a = case a of
    Const _ t -> t
    Var v -> typeOf v
    Comb f _ -> case typeOf f of
      _ :-> a -> a
      _ -> NotWellTyped
    Abs (Variable _ t) b -> t :-> typeOf b

(=.) :: Term -> Term -> Term
a =. b = Comb (Comb (Const "=" $ typeOf a :-> typeOf a :-> Bool) a) b
-- XXX Need to better understand mk_eq.

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
  DEDUCT_ANTISYM (Theorem a1 a2) (Theorem b1 b2) -> validateTheorem (union (delete b2 a1) (delete a2 b1)) $ a2 =. b2
  INST subs (Theorem assumes prop) -> validateTheorem (map (replace subs) assumes) (replace subs prop)
    where
    replace subs a = case a of
      Const _ _ -> a
      Var v -> case lookup v subs of
        Nothing -> a
	Just t  -> t
      Abs v a -> Abs v $ replace [ (v', t) | (v', t) <- subs, v /= v' ] a
      Comb a b -> Comb (replace subs a) (replace subs b)
  -- INST_TYPE           Theorem [(Type, Type)]
  -- AXIOM               Axiom
  _ -> Nothing

-- | Validates (type checks) a theorem.
validateTheorem :: [Term] -> Term -> Maybe Theorem
validateTheorem assumptions conclusion
  | all (\ t -> wellTyped t && typeOf t == Bool) (conclusion : assumptions) = Just $ Theorem assumptions conclusion
  | otherwise = Nothing

-- | Checks if a term is well-typed.
wellTyped :: Term -> Bool
wellTyped a = wellTyped (freeVariables a) a
  where
  wellTyped :: [Variable] -> Term -> Bool
  wellTyped env a = case a of
    Const _ NotWellTyped -> False
    Const _ _ -> True
    Var v -> wellTypedVar env v
    Comb fun arg -> case typeOf fun of
      argType :-> _ -> typeOf arg == argType && wellTyped env fun && wellTyped env arg
      _ -> False
    Abs x t -> wellTyped (x : env) t

  wellTypedVar :: [Variable] -> Variable -> Bool
  wellTypedVar _ (Variable _ NotWellTyped) = False
  wellTypedVar [] _ = False
  wellTypedVar (Variable name t : rest) v@(Variable name' t')
    | name == name' = t == t'
    | otherwise = wellTypedVar rest v

-- | Checks if a variable is free in a term.
freeIn :: Variable -> Term -> Bool
freeIn (Variable name _) t = elem name [ n | Variable n _ <- freeVariables t ]

-- | All free variables in a term.
freeVariables :: Term -> [Variable]
freeVariables = nub . variables []
  where
  variables :: [Variable] -> Term -> [Variable]
  variables env a = case a of
    Const _ _ -> []
    Var v
      | elem v env -> []
      | otherwise -> [v]
    Comb a b -> variables env a ++ variables env b
    Abs x a -> variables (x : env) a

