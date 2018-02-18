{- |
Module      :  Practica1
Maintainer  :  alorozco.patriot53@gmail.com
Stability   :  experimental
Portability :  portable
Version     :  0.1
-}

module Practica1 where


-- Cálculo de proposiciones
data Prop = VarP String            -- Variable proposicional
          | TTrue                  -- T
          | FFalse                 -- ⊥
          | Neg Prop               -- ¬ Φ
          | Conj Prop Prop         -- Φ ^ Ψ
          | Disj Prop Prop         -- Φ v Ψ
          | Imp Prop Prop          -- Φ -> Ψ
          | Equiv Prop Prop        -- Φ <-> Ψ

-- Imprime una fórmula utilizando operadores infijos
instance Show Prop where
  show = error "TBD"

-- Tipo de dato de sustituciones
type Sub = String -> Prop

-- Sustituciones
substitute :: Prop -> Sub -> Prop
substitute = error "TBD"

-- Tipo de dato de estado para variables proposicionales
-- Interpretamos como verdaderas, únicamente y exclusivamentelas variables contenidas
-- en la lista dada como estado
type State = [String]

-- Interpretaciones
interp :: Prop -> State -> Bool
interp = error "TBD"

-- Dado un estado, ¿será un modelo para una fórmula?
model :: Prop -> State -> Bool
model = error "TBD"

-- Devuelve todas las variables contenidas en una fórmula dada
vars :: Prop -> [String]
vars = error "TBD"
  
-- Genera la lista (conjunto) potencia de una lista
powerSet :: Eq a => [a] -> [[a]]
powerSet = error "TBD"

-- ¿Es tautología?
tautology :: Prop -> Bool
tautology = error "TBD"

-- Equivalencia de fórmulas
equivProp :: Prop -> Prop -> Bool
equivProp = error "TBD"

-- Consecuencia lógica
logicConsequence :: [Prop] -> Prop -> Bool
logicConsequence = error "TBD"
