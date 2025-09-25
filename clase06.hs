--RECURSION SOBRE LISTAS
takeN :: Int -> [a] -> [a]
takeN 0 _ = []
takeN n [] = error "No hay suficientes elementos" 
takeN n (x:xs) = x:(takeN (n-1) xs)

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = error "No hay suficientes elementos"
myDrop n (x:xs) = myDrop (n-1) xs

--Es mejor manejar errores con tipos seguros, pero son fundamentos que no veremos aqui.
--Si les llama la atencion, pueden investigar el tipo Maybe y monadas en general.


--Funcion de orden superior
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x):(myMap f xs)
 
--El razonamiento de quicksort puede que les sirva para la funcion del conjunto potencia
{-
Agarramos un pivote, lo mandamos a la "mitad".

Despues, hacemos dos sublistas como sigue:
pasamos todo lo que sea menor o igual al pivote del lado izquierdo y 
todo lo que sea mayor del lado derecho

Finalmente, ordenamos recursivamente esas sublistas.

Observen que el pivote ya queda en su lugar. Es decir, ya queda ordenado.
-}
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = (quickSort [y | y <- xs, y <= x ]) ++ [x] ++ (quickSort [y | y <- xs, y > x ])