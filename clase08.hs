{-
Vamos a definir un arbol binario con nodos etiquetados por elementos de tipo A
como sigue:

+ Un arbol vacio es un arbol binario de tipo A. Representado por Void o Nil
dependiendo de la bibliografia

+ Si t_1 y t_2 son arboles binarios de tipo A y x es un elemento de tipo A,
entonces tree(t1,x,t2) es un arbol binario de tipo A.

+ Son todos.
-}
data ArbolB a = Void | Node (ArbolB a) a (ArbolB a) 
                deriving (Show, Eq) 

--Ejemplo de uso
t1, t2, t3, t4 :: ArbolB Int

t4 = (Node Void 4 Void)
t3 = (Node  Void 3 t4)
t2 = (Node Void 2 Void)
t1 = (Node t2 1 t3)

-- ¿Como se ve t1?

--Para revisar cuantos elementos tiene un arbol binario
numeroElementos :: ArbolB a -> Int
numeroElementos Void = 0
numeroElementos (Node t1 x t2) = 1 + numeroElementos t1 + numeroElementos t2

--Si quiero sacar la altura del arbol
maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

altura :: ArbolB a -> Int
altura Void = 0
altura (Node t1 _ t2) = 1 + maximo (altura t1) (altura t2)

-- Si quiero sumar los elementos del arbol con tipos numericos
sumaElementos :: Num a => ArbolB a -> Int
sumaElementos Void = 0
sumaElementos (Node t1 x t2) = 1 + sumaElementos t1 + sumaElementos t2


--Buscar elementos en un árbol
busca :: Eq a => ArbolB a -> a -> Bool
busca Void _ = False
busca (Node t1 x t2) y = if x == y
                        then True
                        else if busca t1 y 
                            then True
                            else busca t2 y 

-- Opcion alterna que mencionaron en clase
busca2 :: Eq a => ArbolB a -> a -> Bool
busca2 Void _ = False
busca2 (Node t1 x t2) y = if x == y
                        then True
                        else (busca2 t1 y) || (busca2 t2 y)

-- Sacar el espejo de un arbol binario
espejo :: ArbolB a -> ArbolB a
espejo Void = Void
espejo (Node t1 x t2) = (Node (espejo t2) x (espejo t1))


-- Recorridos en arboles binarios
preorden :: ArbolB a -> [a]
preorden Void = []
preorden (Node t1 x t2) = [x] ++ preorden t1 ++ preorden t2

inorden :: ArbolB a -> [a]
inorden Void = []
inorden (Node t1 x t2) = inorden t1 ++ [x] ++ inorden t2

postorden :: ArbolB a -> [a]
postorden Void = []
postorden (Node t1 x t2) = postorden t1 ++ postorden t2 ++ [x]