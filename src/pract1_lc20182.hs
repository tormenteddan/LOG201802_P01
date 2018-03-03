{- |
Module      :  Practica1
Maintainer  :  alorozco.patriot53@gmail.com
Stability   :  experimental
Portability :  portable
Version     :  0.1
-}
module Practica1 where

import           Data.List

-- Cálculo de proposiciones
data Prop = Var String            -- Variable proposicional
          | TTrue                 -- T
          | FFalse                -- ⊥
          | Neg Prop              -- ¬ Φ
          | Conj Prop Prop        -- Φ ^ Ψ
          | Disy Prop Prop        -- Φ v Ψ
          | Impl Prop Prop        -- Φ -> Ψ
          | Equi Prop Prop        -- Φ <-> Ψ

-- Tipo de dato de sustituciones
type Sub = String -> Prop

-- Tipo de dato de estado para variables proposicionales
-- Interpretamos como verdaderas, únicamente y exclusivamentelas variables contenidas
-- en la lista dada como estado
type State = [String]

-- Imprime una fórmula utilizando operadores infijos
instance Show Prop where
  show f = case f of
    TTrue -> "⊤"
    FFalse -> "⊥"
    (Var x) -> x
    (Neg f1) -> case f1 of
        TTrue        -> "'⊤"
        FFalse       -> "'⊥"
        (Var x)      -> "'" ++ x
        (Neg n1)     -> "'" ++ show n1
        (Disy n1 n2) -> "'" ++ show (Disy n1 n2)
        (Conj n1 n2) -> "'" ++ show (Conj n1 n2)
        (Impl n1 n2) -> "'" ++ show (Impl n1 n2)
        (Equi n1 n2) -> "'" ++ show (Equi n1 n2)
    (Disy f1 f2) -> "(" ++ show f1 ++ " V " ++ show f2 ++ ")"
    (Conj f1 f2) -> "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
    (Impl f1 f2) -> "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
    (Equi f1 f2) -> "(" ++ show f1 ++ " <-> " ++ show f2 ++ ")"

-- Sustituciones
substitute :: Prop -> Sub -> Prop
substitute f subs = case f of
    TTrue        -> TTrue
    FFalse       -> FFalse
    (Var p)      -> subs p
    (Neg p)      -> Neg $ substitute p subs
    (Disy p1 p2) -> substitute p1 subs `Disy` substitute p2 subs
    (Conj p1 p2) -> substitute p1 subs `Conj` substitute p2 subs
    (Impl p1 p2) -> substitute p1 subs `Impl` substitute p2 subs
    (Equi p1 p2) -> substitute p1 subs `Equi` substitute p2 subs

-- | The biconditional operator can be read as logical equivalence.
(<->) :: Bool -> Bool -> Bool
(<->) = (==)

-- | Φ -> Ψ is equivalent to ¬Φ v Ψ
(-->) :: Bool -> Bool -> Bool
(-->) phi psi = not phi || psi

-- Interpretaciones
interp :: State -> Prop -> Bool
interp _ TTrue      = True
interp _ FFalse     = False
interp m (Var v)    = v `elem` m
interp m (Neg  p)   = not (interp m p)
interp m (Conj p q) = interp m p && interp m q
interp m (Disy p q) = interp m p || interp m q
interp m (Impl p q) = interp m p --> interp m q
interp m (Equi p q) = interp m p <-> interp m q

-- Dado un estado, ¿será un modelo para una fórmula?
model :: State -> Prop -> Bool
model = interp

-- Devuelve todas las variables contenidas en una fórmula dada
vars :: Prop -> [String]
vars = dedup . vars' [] where
    vars' acc (Var f)      = f:acc
    vars' acc (Neg f)      = vars' acc f
    vars' acc (Disy f1 f2) = vars' (vars' acc f1) f2
    vars' acc (Conj f1 f2) = vars' (vars' acc f1) f2
    vars' acc (Impl f1 f2) = vars' (vars' acc f1) f2
    vars' acc (Equi f1 f2) = vars' (vars' acc f1) f2
    vars' acc _            = acc

-- Genera la lista (conjunto) potencia de una lista
powerSet :: Ord a => [a] -> [[a]]
powerSet ss = [] : powerSetAux (dedup ss)
    where powerSetAux []      =  []
          powerSetAux (x:xs)  =  [x] : foldr f [] (powerSetAux xs)
            where f curr rest = curr : (x : curr) : rest

-- | La función `dedup` remueve duplicados de una lista ordenable.
-- Hace esto ordenando la lista, agrupando elementos iguales y
-- extrayendo el primero de cada uno de estos grupos.
dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort

-- ¿Es tautología?
tautology :: Prop -> Bool
tautology p = and [interp m p | m <- powerSet $ vars p]

-- Equivalencia de fórmulas
equivProp :: Prop -> Prop -> Bool
equivProp p1 p2 = tautology $ Equi p1 p2

-- Consecuencia lógica
logicConsequence :: [Prop] -> Prop -> Bool
logicConsequence ps c = not $ or [interp m hypotheses | m <- powerSet $ vars hypotheses]
    where hypotheses = foldr Conj (Neg c) ps
