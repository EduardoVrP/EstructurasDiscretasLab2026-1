--Recursion
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

--Para definir el tipo de un conjunto de datos, hacemos los siguiente:
-- data NombreDelTipo = ElementoBase1 | ElementoBase2 | ... |ElementoBaseN|
--                      | NombreConsructor Parametro1 Parametro2 ... ParametroM
-- NombreDelTipo es el nombre de nuestro conjunto de datos por ejemplo:
-- Integer, Float, Bool, etc
--ElementoBase1,..., ElementoBaseN son los elementos más basicos de nuestro conjunto.
--data Bool = True | False
--NombreConstructor es el nombre con el que se identifica 
--un elemento contruido de tipo NombreDelTipo
--en este caso NombreConstructor es Suc y puede recibir más de un parámetro.
-- Parametro1, ..., ParametroM son los tipos del Parametro.
-- Por ejemplo el tipo maybe ...

-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y para poder comparar, luego indagamos mas en eso

valorNumerico :: Natural -> Integer
valorNumerico Cero = 0
valorNumerico (Suc n) = 1 + valorNumerico n

factorialNat :: Natural -> Natural
factorialNat Cero = (Suc Cero)
factorialNat (Suc n) = multiplicacion (Suc n) (factorialNat n) --Necesito la multiplicacion

multiplicacion :: Natural -> Natural -> Natural
multiplicacion Cero m = Cero
multiplicacion (Suc Cero) m = m
multiplicacion (Suc n) m = suma m (multiplicacion n m) --necesito la suma
--multiplicacion (Suc n) m = suma n (multiplicacion n m) MAL HECHO

suma :: Natural -> Natural -> Natural
suma Cero m = m
suma (Suc n) m = Suc (suma n m)

resta :: Natural -> Natural -> Natural
resta n       Cero     = n
resta Cero    _        = Cero
resta (Suc m) (Suc n)  = resta m n

--RECURSION MAL HECHA
--Usaremos unicamente potencias de dos
log2 :: Float -> Float
log2 0 = 0 --Si ponemos log2 1 = 0 se soluciona 
log2 n = (log2 (n / 2)) + 1 --Como tal no se llega al caso base


-- Idea refinada
-- Queremos contar cuantas veces se puede dividir entre 2
numDivDos :: Int -> Int
numDivDos 0 = 0 --Estamos contando la division 1/n, hay que cambiar el caso base
numDivDos n = 1 + numDivDos (n `div` 2)