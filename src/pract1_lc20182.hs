{- |
Module      :  Propositional
Author      :  Luis Daniel Aragon Bermudez
Maintainer  :  daniel.aragon@ciencias.unam.mx
Stability   :  experimental
Portability :  portable
Version     :  0.1
-}
module Propositional(
    -- * Logical proposition
    Prop,
    -- * Logical state
    State,
    -- * Propositional calculus substitution
    Sub,
    -- ** Logical calculus functions
    substitute,
    interp,
    model,
    vars,
    tautology,
    equivProp,
    logicConsequence
    ) where

import           Data.List

infixl 5 :&&
infixl 5 :||
infixr 4 :->
infixl 3 :<>

-- | Prop datatype. Defines a proposition in propositional calculus.
data Prop = Var String    -- ^ Propositional variable
          | Const Bool    -- ^ Propositional constant
          | Neg Prop      -- ^ Negation, ¬P
          | Prop :&& Prop -- ^ Conjunction, P ∧ Q
          | Prop :|| Prop -- ^ Disyunction, P ∨ Q
          | Prop :-> Prop -- ^ Conditional, P -> Q
          | Prop :<> Prop -- ^ Biconditional, P <-> Q

-- | Sub type. Functions that map propositional variables (by their
-- constructing string) within a proposition to other propositions.
--   Examples:
--       The substitution [p:=p->q] can be specified by the `Sub` s:
--       s "p"   = Var "p" :-> Var "q"
--       s other = Var other
type Sub = String -> Prop

-- | State type. Denotes a mapping of propositional variables to truth values.
-- A propositional variable `Var s` should be mapped to True  by an
-- interpretation iif `s` is contained in its "generating" state
type State = [String]

instance Show Prop where
  show f = case f of
    Const bool -> if bool then "\8868" else "\8869"
    (Var x) -> x
    (Neg f1) -> case f1 of
        (Const bool) -> if bool then "NOT \8868" else "NOT \8869"
        (Var x)      -> "NOT " ++ x
        (Neg n1)     -> "NOT " ++ show n1
        other        -> "NOT " ++ show other
    (f1:||f2) -> "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    (f1:&&f2) -> "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
    (f1:->f2) -> "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
    (f1:<>f2) -> "(" ++ show f1 ++ " <-> " ++ show f2 ++ ")"

-- |substitute. Performs the substitution provided as its first argument on the
-- proposition provided as its second argument
substitute :: Prop -> Sub -> Prop
substitute f subs = case f of
    (Var p)   -> subs p
    (Neg p)   -> Neg $ substitute p subs
    (p1:||p2) -> substitute p1 subs :|| substitute p2 subs
    (p1:&&p2) -> substitute p1 subs :&& substitute p2 subs
    (p1:->p2) -> substitute p1 subs :-> substitute p2 subs
    (p1:<>p2) -> substitute p1 subs :<> substitute p2 subs
    other     -> other

-- |interp. Recursively evaluates the truth value of a proposition given a state
interp :: State -> Prop -> Bool
interp _ (Const bool) = bool
interp m (Var v)      = v `elem` m
interp m (Neg  p)     = not (interp m p)
interp m (p:&&q)      = interp m p && interp m q
interp m (p:||q)      = interp m p || interp m q
interp m (p:->q)      = not (interp m p) || interp m q
interp m (p:<>q)      = interp m p == interp m q

-- |model. Given a state and a proposition, determines if said state is a model
-- of said proposition.
model :: State -> Prop -> Bool
model = interp

-- |vars. Returns the set (as a list) of variables within a proposition.
vars :: Prop -> [String]
vars = set . vars' [] where
    vars' acc (Var f)   = f:acc
    vars' acc (Neg f)   = vars' acc f
    vars' acc (f1:||f2) = vars' (vars' acc f1) f2
    vars' acc (f1:&&f2) = vars' (vars' acc f1) f2
    vars' acc (f1:->f2) = vars' (vars' acc f1) f2
    vars' acc (f1:<>f2) = vars' (vars' acc f1) f2
    vars' acc _         = acc

-- |tautology. Determines if a proposition is a tautology, i.e. every interpretation
-- of said proposition returns true.
tautology :: Prop -> Bool
tautology p = and [interp m p | m <- powerSet $ vars p]

-- |equivProp. Determines if two propositions are equivalent. By the rules of
-- propositional logic: φ ≡ ψ ⇔ ⊧ φ ↔ ψ
equivProp :: Prop -> Prop -> Bool
equivProp p1 p2 = tautology (p1:<>p2)

-- |logicConsequence. Determines if the proposition provided as the second
-- argument is a logical consequence of the premises (list of propositions)
-- provided as the first argument. It does this by applying the refutation
-- principle: Γ ⊧ φ ⇔ Γ ∪ ¬ {φ} is unsatisfiable.
logicConsequence :: [Prop] -> Prop -> Bool
logicConsequence ps c = not $
    or [interp m hypotheses | m <- powerSet $ vars hypotheses]
        where hypotheses = foldr (:&&) (Neg c) ps

-- |set. Given a list of ordered datatype, it removes duplicates by ordering
-- the list, grouping like elements and extracting the first element of each
-- group into a list, effectively converting it into a sort of set derived from
-- the original list.
set :: Ord a => [a] -> [a]
set = map head . group . sort

-- |powerSet. Given a list of ordered datatype, it returns all the possible
-- subsets of its set version. Effectively it returns its power set.
powerSet :: Ord a => [a] -> [[a]]
powerSet ss = [] : powerSetAux (set ss)
    where powerSetAux []      =  []
          powerSetAux (x:xs)  =  [x] : foldr f [] (powerSetAux xs)
            where f curr rest = curr : (x : curr) : rest
