--Recordemos estas funciones
takeN :: Int -> [a] -> [a]
takeN 0 _ = []
takeN n [] = error "No hay suficientes elementos" 
takeN n (x:xs) = x:(takeN (n-1) xs)

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = error "No hay suficientes elementos"
myDrop n (x:xs) = myDrop (n-1) xs

long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs


--Funcion halves
halves :: [a] -> ([a], [a])
halves xs = (takeN mitad xs, myDrop mitad xs)
  where
    mitad = long xs `div` 2


--Funcion putMid
putmid :: a -> [a] -> [a]
putmid x xs = ys ++ (x : zs)
  where
    (ys, zs) = halves xs


--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

--Para que se vea bien en la terminal
instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


--Algunas variables para no estar escribiendo mucho en la terminal
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

-- Contar variables en una formula
contarVariables :: Prop -> Int
contarVariables (Var p) = 1
contarVariables (Cons _) = 0
contarVariables (Not phi) = contarVariables phi
contarVariables (Or phi psi) = contarVariables phi + contarVariables psi
contarVariables (And phi psi) = contarVariables phi + contarVariables psi
contarVariables (Impl phi psi) = contarVariables phi + contarVariables psi
contarVariables (Syss phi psi) = contarVariables phi + contarVariables psi

-- Contar los conectivos de una fórmula
contarConectivos :: Prop -> Int
contarConectivos (Var _) = 0
contarConectivos (Cons _) = 0
contarConectivos (Not phi) = 1 + contarConectivos phi
contarConectivos (And phi psi) = 1 + contarConectivos phi + contarConectivos psi
contarConectivos (Or phi psi) = 1 + contarConectivos phi + contarConectivos psi
contarConectivos (Impl phi psi) = 1 + contarConectivos phi + contarConectivos psi
contarConectivos (Syss phi psi) = 1 + contarConectivos phi + contarConectivos psi