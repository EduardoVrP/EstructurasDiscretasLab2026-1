--Comentario de una linea
{-
Comentario multilinea
-}

--Todo es una funcion en el sentido matemático
cuadratica :: Int -> Int
cuadratica x = x^2

--Incluso las constantes, son funciones que no reciben nada y siempre devuelven lo mismo
constante :: Int
constante = 10

--Si necesito mas agumentos
multiplicacion :: Int -> Int -> Int
multiplicacion a b = a*b

--El modulo es muy importante en computacion
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = n `mod` m == 0

--Booleanos con if
andb :: Bool -> Bool -> Bool
andb x y = if x then y else False

orb :: Bool -> Bool -> Bool 
orb x y = if x then True else y

notb :: Bool -> Bool 
notb x = if x then False else True

--Maximo con if
maximo :: Int -> Int -> Int
maximo x y = if x >= y then x else y


--Por el momento y para los propositos de esta clase, veamos las listas como si fueran conjuntos
--Esto de aqui es potencialmente infinito
naturales :: [Int]
naturales = [0,1..]

--Dar los pares menores al numero dado
--Funciona pero se queda trabajando
pares :: Int -> [Int]
pares n = [x | x <- naturales, x<n, esMultiplo x 2]

--RELACIONES
type Rel a b = [(a, b)]

menorQue :: Int -> Int -> Bool
menorQue x y = x < y

--aRb si a es menor que b
relacionMenor :: Rel Int Int
relacionMenor = [(x, y) | x <- [1..9], y <- [1..4], menorQue x y]

--aRb si a es multiplo de y
relacionMultiplo :: Rel Int Int
relacionMultiplo = [(x,y) | x <- [1..9], y <- [1..4], esMultiplo x y]