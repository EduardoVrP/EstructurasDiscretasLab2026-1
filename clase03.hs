--Haremos un primer intento de construir cadenas binarias
{-Por facilidad y por el momento, estaremos pensando los numeros al reves; 
esto es, el número 6_10 = 110_2 se representa como 011-}
data CadenaBinaria = Vacia 
            | O CadenaBinaria 
            | I CadenaBinaria 
            deriving (Show, Eq)

--BINARIO A DECIMAL
--Problema: necesito llevar la cuenta de las potencias de dos
-- Solucion: una funcion auxiliar que me permita llevar la potencia
toDecimal :: CadenaBinaria -> Int
toDecimal bin = toDecimalAux bin 0

toDecimalAux :: CadenaBinaria -> Int -> Int
toDecimalAux Vacia _ = 0
toDecimalAux (O bin) n = 0*(2^n) + (toDecimalAux bin (n+1)) --Notese que no es necesario el primer sumando, solo la llamada recursiva
toDecimalAux (I bin) n = 1*(2^n) + (toDecimalAux bin (n+1))
--Necesitamos incrementar las potencias de dos en cada llamada recursiva


--Definicion alterna vista en clase de teoria
toDecimal2 :: CadenaBinaria -> Int
toDecimal2 Vacia = 0
toDecimal2 (I resto) = 2*(toDecimal2 resto) + 1
toDecimal2 (O resto) = 2*(toDecimal2 resto) + 0

--DECIMAL A BINARIO
toBin :: Int -> CadenaBinaria
toBin 0 = Vacia
toBin n = if n `mod` 2 == 0 
    then O (toBin (n `div` 2))
    else I (toBin (n `div` 2))
{-¿Se puede organizar mejor el codigo?-}

toBin2 :: Int -> CadenaBinaria
toBin2 0 = Vacia
toBin2 n = if r == 0 
    then O (resto)
    else I (resto)
    where 
        (q,r) = divMod n 2
        resto = toBin2 q

--SUMA
incremento :: CadenaBinaria -> CadenaBinaria
incremento Vacia = I Vacia
incremento (O x) = I x
incremento (I x) = O (incremento x)

suma :: CadenaBinaria -> CadenaBinaria -> CadenaBinaria
suma Vacia y = y
suma x Vacia = x
suma (O x) (O y) = O (suma x y)
suma (O x) (I y) = I (suma x y)
suma (I x) (O y) = I (suma x y)
suma (I x) (I y) = O (incremento (suma x y))