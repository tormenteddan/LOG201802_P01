{- |
Module      :  EjercicioSemanal1
Stability   :  experimental
Portability :  portable
Version     :  0.1
-}

module EjercicioSemanal1 where

-- Listas

-- Reversa de una lista
myReverse :: [a] -> [a]
myReverse = error "to be implemented..."

-- Toma los primeros elementos de la lista pasada como
-- segundo argumento de acuerdo al primer argumento
myTake :: Int -> [a] -> [a]
myTake = error "to be implemented..."

-- Cuenta cuántas veces se repite el primer argumento en la lista dada
myCount :: Eq a => a -> [a] -> Int
myCount = error "to be implemented..."

-- Obtiene una lista que indica cuántas veces se repiten
-- los elementos de la lista dada
myFreq :: Eq a => [a] -> [(a, Int)]
myFreq = error "to be implemented..."

-- Dada una lista l y dos enteros i, j, devuelve la sublista que empieza
-- en el i-ésimo elemento de l y termina en el j-1-ésimo
-- Análogo del list[i:j] de Python
range :: [a] -> Int -> Int -> [a]
range = error "to be implemented..."

-- Dada una lista de tipo a y un elemento de tipo a, devuelve la lista
-- de índices en los que dicho elemento ocurre en la lista
indexEq :: Eq a => [a] -> a -> [Int]
indexEq = error "to be implemented..."

-- Dada una lista de tipo a y un elemento de tipo a, devuelve la lista de listas
-- formada por todas las sublistas que preceden y suceden las apariciones del elemento
-- Por ejemplo:
-- split "the quick brown fox jumps over the lazy dog" ' ' =
-- ["the","quick","brown","fox","jumps","over","the","lazy","dog"]
-- Análogo del list.split() de Python
split :: Eq a => [a] -> a -> [[a]]
split = error "to be implemented..."

-- Números naturales

data Nat = Zero | Succ Nat  deriving (Show, Eq)

-- Suma de naturales (auxiliar)
sumNat :: Nat -> Nat -> Nat
sumNat = error "to be implemented..."

-- Producto de números naturales
prodNat :: Nat -> Nat -> Nat
prodNat = error "to be implemented..."

-- Calcula el número natural resultante de elevar el primer argumento
-- a la potencia del segundo argumento
powerNat :: Nat -> Nat -> Nat
powerNat = error "to be implemented..."

-- Decide si los números pasados como argumentos son iguales
eqNat :: Nat -> Nat -> Bool
eqNat = error "to be implemented..."

-- Mayor estricto (auxiliar)
greaterThan :: Nat -> Nat -> Bool
greaterThan = error "to be implemented..."

-- Convierte el Nat pasado a Int
natToInt :: Nat -> Int
natToInt = error "to be implemented..."

-- Convierte el Int pasado a Nat
intToNat :: Int -> Nat
intToNat = error "to be implemented..."

-- Quita el sufijo de tamaño k de una lista (los últimos k elementos de una lista)
throwRev :: Nat -> [a] -> [a]
throwRev = error "to be implemented..."

-- "Comprime" la cadena dada concetenando los prefijos de longitud a lo más la mitad de la
-- longitud de cada palabra
-- Por ejemplo:
-- dumbCompress "the quick brown fox jumps over the lazy dog" =
-- "tqubrfjuovtlad"
dumbCompress :: String -> String
dumbCompress = error "to be implemented..."
