--Listas de elementos de tipo T
{-
+ La lista vacia [] es una lista de elementos de tipo T
+ Si x es un elemento de tipo T y xs es una lista de elementos
    de tipo t, entonces x:xs es una lista de elementos de tipo T.
+ Son todas
-}

--Para verificar si un entero esta en una lista de enteros
contieneEnteros :: [Int] -> Int -> Bool
contieneEnteros [] _ = False
contieneEnteros (x:xs) y = if x == y then True else contieneEnteros xs y


--Generalizacion de la funcion anterior a listas de cualquier tipo
--Necesitamos que se pueda comparar.
contiene :: (Eq a) => [a] -> a -> Bool
contiene [] _ = False
contiene (x:xs) y = if x == y then True else contiene xs y

--Longitud de una lista
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

--El operador ++ concatena dos listas dadas
agregaFinal :: [a] -> a -> [a]
agregaFinal xs x = xs ++ [x]

--Como implementamos ++?
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = (x:concatena xs ys)

--Reversa de una lista
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = concatena (reversa xs) [x] --O bien reversa xs ++ [x]

--Implementacion propia
data Lista a = Vacia |
            Cons a (Lista a)
            deriving (Show,Eq)

contiene2 :: (Eq a) => Lista a -> a -> Bool
contiene2 Vacia _ = False
contiene2 (Cons x (xs)) y = if x == y then True else contiene2 xs y

concatena2 :: Lista a -> Lista a -> Lista a 
concatena2 Vacia ys = ys
concatena2 (Cons x (xs)) ys = (Cons x (concatena2 xs ys))

reversa2 :: Lista a -> Lista a
reversa2 Vacia = Vacia
reversa2 (Cons x (xs)) = concatena2 (reversa2 xs) (Cons x Vacia)


--LISTAS POR COMPRENSION

--Las usamos con las relaciones

par :: Int -> Bool
par n = mod n 2 == 0

filtroPares :: [Int] -> [Int]
filtroPares [] = []
filtroPares (x:xs) = if par x then x:(filtroPares xs) else filtroPares xs

filtroParesComprension :: [Int] -> [Int]
filtroParesComprension xs = [x | x <- xs, par x]

--TAREA MORAL: investigar la funcion filter

{-
Se sigue lo siguiente:
[expresion | generador, condicion1, condicion2,..., condicionN]
-}

--Primeros 10 numeros naturales

primerosPares :: [Int]
primerosPares = [2*x | x <- [0..9]]

primerosPares2 :: [Int]
primerosPares2 = [x | x <- [0..18], par x]