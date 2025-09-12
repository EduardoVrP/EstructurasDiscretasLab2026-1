{-
Figuras geometricas con los siguientes parametros:
    + El circulo recibe el radio
    + El cuadrado recibe un lado
    + El rectangulo recibe los dos lados
    + El triangulo es equilatero y recibe el lado
-}
data Shape = Circle Float |
            Square Float | 
            Rectangle Float Float| 
            Triangle Float 
            deriving (Show)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square l) = l * l
area (Rectangle b h) = b * h
area (Triangle l) = ((sqrt 3 ) * l^2)/4

--Funcion que calcula el perimetro de las figuras
perimetro :: Shape -> Float
perimetro (Circle r) = pi * r * 2
perimetro (Square l) = 4 * l
perimetro (Rectangle b h) = (2 * b) + (2 * h)
perimetro (Triangle l) = 3 * l

-- Hacer que Shape pueda ordenarse con respecto al perimetro
instance Ord Shape where
    compare s1 s2 = compare (perimetro s1) (perimetro s2)

-- Hacer que Shape sea pueda comparar con respecto al area
instance Eq Shape where
    s1 == s2 = area s1 == area s2