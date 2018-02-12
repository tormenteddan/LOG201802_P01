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
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Toma los primeros elementos de la lista pasada como
-- segundo argumento de acuerdo al primer argumento
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake k list = case list of
                  [] -> []
                  (x:xs) -> [x] ++ (myTake (k-1) xs)

-- Cuenta cuántas veces se repite el primer argumento en la lista dada
myCount :: Eq a => a -> [a] -> Int
myCount _ [] = 0
myCount e (x:xs) = if x == e
                   then 1 + (myCount e xs)
                   else myCount e xs

-- Obtiene una lista que indica cuántas veces se repiten
-- los elementos de la lista dada
myFreq :: Eq a => [a] -> [(a, Int)]
myFreq list = [(e, myCount e list) | e <- list]

-- Dada una lista l y dos enteros i, j, devuelve la sublista que empieza
-- en el i-ésimo elemento de l y termina en el j-1-ésimo
-- Análogo del list[i:j] de Python
range :: [a] -> Int -> Int -> [a]
range list n m
  | m <= n = []
  | otherwise = rangeAux 0 list
  where
    rangeAux _ [] = []
    rangeAux i (x:xs)
      | i < n = rangeAux (i+1) xs
      | i == n = x : (rangeAux (i+1) xs)
      | i < m = x : (rangeAux (i+1) xs)
      | i >= m = []

-- Dada una lista de tipo a y un elemento de tipo a, devuelve la lista
-- de índices en los que dicho elemento ocurre en la lista
indexEq :: Eq a => [a] -> a -> [Int]
indexEq [] _ = []
indexEq list c = [i | i <- [0..len], (list !! i) == c]
  where
    len = case length list > 0 of
            True -> (length list) - 1
            False -> 0

-- Otra versión del indexEq
indexEqNoe :: Eq a => [a] -> a -> [Int]
indexEqNoe [] _ = []
indexEqNoe list c = [i | i <- [0..len], (list !! i) == c]
  where
    len = (length list) - 1

-- Dada una lista de tipo a y un elemento de tipo a, devuelve la lista de listas
-- formada por todas las sublistas que preceden y suceden las apariciones del elemento
-- Por ejemplo:
-- split "the quick brown fox jumps over the lazy dog" ' ' =
-- ["the","quick","brown","fox","jumps","over","the","lazy","dog"]
-- Análogo del list.split() de Python
split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split list char = splitAux [] list
  where
    splitAux acc [] = [acc]
    splitAux acc (x:xs)
      | char /= x = splitAux (acc ++ [x]) xs
      | otherwise = acc : (splitAux [] xs)

-- Números naturales

data Nat = Zero | Succ Nat  deriving (Show, Eq)

-- Suma de naturales (auxiliar)
sumNat :: Nat -> Nat -> Nat
sumNat Zero m = m
sumNat (Succ n) m = Succ $ sumNat n m

-- Producto de números naturales
prodNat :: Nat -> Nat -> Nat
prodNat Zero _ = Zero
prodNat (Succ n) m = sumNat (prodNat n m) m

-- Calcula el número natural resultante de elevar el primer argumento
-- a la potencia del segundo argumento
powerNat :: Nat -> Nat -> Nat
powerNat _ Zero = Succ Zero
powerNat n (Succ m) = prodNat (powerNat n m) n

-- Decide si los números pasados como argumentos son iguales
eqNat :: Nat -> Nat -> Bool
eqNat Zero m = case m of
                 Zero -> True
                 Succ _ -> False
eqNat (Succ n) m = case m of
                     Zero -> False
                     Succ y -> eqNat n y

-- Mayor estricto (auxiliar)
greaterThan :: Nat -> Nat -> Bool
greaterThan Zero _ = False
greaterThan (Succ n) m = case m of
                           Zero -> True
                           Succ y -> greaterThan n y

-- Convierte el Nat pasado a Int
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- Convierte el Int pasado a Nat
intToNat :: Int -> Nat
intToNat n
  | n < 0 = error "not a natural number!"
  | n == 0 = Zero
  | otherwise = Succ $ intToNat $ n-1

-- Quita el sufijo de tamaño k de una lista (los últimos k elementos de una lista)
throwRev :: Nat -> [a] -> [a]
throwRev n l = myReverse $ throw n (myReverse l)
  where
    throw Zero l = l
    throw (Succ n) l = case l of
                         [] -> []
                         (x:xs) -> throw n xs

-- Otra versión del throwRev
throwRevNoe :: Nat -> [a] -> [a]
throwRevNoe n l = myTake m l
  where
    val = length l - natToInt n
    m = if val >= 0
        then val
        else 0

-- "Comprime" la cadena dada concetenando los prefijos de longitud a lo más la mitad de la
-- longitud de cada palabra
-- Por ejemplo:
-- dumbCompress "the quick brown fox jumps over the lazy dog" =
-- "tqubrfjuovtlad"
dumbCompress :: String -> String
dumbCompress str = join [range s 0 (div (length s) 2) | s <- split str ' ']
  where
    join [] = ""
    join (x:xs) = x ++ (join xs)
