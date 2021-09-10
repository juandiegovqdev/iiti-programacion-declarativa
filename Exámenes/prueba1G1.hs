{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- EJERCICIO 1. COMPRENSION
-- Una lista es genial si la diferencia en valor absoluto entre
-- cualesquiera dos terminos consecutivos es siempre mayor o igual que
-- la posicion del primero de los dos. Por ejemplo,
-- genial [1,3,-4,1] == True
-- Ya que |1-3| = 2 >= 0 = posicion del 1,  
-- |3-(-4)|= 7 >= 1 posicion del 3,
-- |(-4)-1|= 5 >= 2 posicion del -4. 
-- genial [1,3,0,1,2] == False
-- Ya que |3-1| = 2 >=0; |0-3|=3>=1; pero |1-0|=1 no es >=2.
-- Definir por comprension una funcion (genial xs) tal que reconozca  si
-- xs es una lista genial.

genial :: [Int] -> Bool
genial xs = all (==True) [abs (xs!!x-xs!!(x+1)) > x | x <- [0..length xs-2]]

-- Solución usando recursión.
genialRec :: [Int] -> Bool
genialRec = genialAux 0

genialAux :: Int -> [Int] -> Bool
genialAux a (x:y:xs)
    | abs (x-y) > a = True && genialAux (a+1) (y:xs)
    | otherwise = False
genialAux _ (x:xs) = True
genialAux _ [] = True

-- ---------------------------------------------------------------------------
-- EJERCICIO 2. USANDO COMPRENSION / USANDO RECURSION
-- Representamos un polinomio como la lista de los pares 
-- [(a_1, n_1), ... ,(a_m,n_m)]  donde en cada par, el a_i es el
-- coeficiente del monomio y n_i el grado. 
-- Por ejemplo, el polinomio 
-- 5x^3 - 3x^2 - x - 1 se representara por
-- [(5,3),(-3,2),(-1,1),(-1,0)]

-- EJERCICIO 2.1.
-- Definir usando recursion la funcion valeR tal que 
-- (valeR ps x) devuelve el valor de polinomio ps para x.
-- Definir usando recursion la funcion esRaizR tal que 
-- (esRaizR ps x) comprueba si x es una raiz del polinomio ps.
-- Por ejemplo:
-- valeR [(2,2),(-2,1),(1,0)] (-1) == 5
-- esRaizR [(5,3),(-3,2),(-1,1)] 0 == True
-- esRaizR [(-1,2),(1,0)] 1 == True
-- esRaizR [(-1,3),(1,0)] 0 == False

valeR :: [(Float, Float)] -> Float  ->  Float
valeR ((x, y):xs) a = x * (a**y) + valeR xs a
valeR [] _ = 0

esRaizR = undefined

-- EJERCICIO 2.2.
-- Definir la funcion valeC, que actue como valeR, usando
-- comprension. Define esRaizC usandola.

valeC :: [(Float, Float)] -> Float  -> Float
valeC xs a = sum [x * (a**y) | (x, y) <- xs]

esRaizC = undefined

-- EJERCICIO 2.3.
-- Definir una propiedad para comprobar con quickCheck que esRaizR y
-- esRaizC son iguales.

prop_raiz = undefined

-- ---------------------------------------------------------------------------
-- EJERCICIO 3.   COMPRENSION
-- Dos numeros no primos a y b, son parientes si la
-- suma de los divisores propios de a coincide con el producto de los
-- divisores propios de b. Por ejemplo, 8 y 49 son parientes porque los
-- divisores propios de 8 son 1,2,4; los divisores propios de 49 son 1 y
-- 7; la suma 1+2+4 = 1*7. Sin embargo, 49 y 8 no son parientes, ya que
-- 1+7 no es igual a 1*2*4. 
-- EJERCICIO 3.1.
-- Define la funcion sonParientes, tal que (sonParientes x y) compruebe
-- si dos numeros cualesquiera x e y son parientes. Por ejemplo:
-- sonParientes 8 49 == True
-- sonParientes 25 6 == True
-- sonParientes 6 25 == False

sonParientes = undefined

-- EJERCICIO 3.2.
-- Define la funcion parientes, tal que (parientes n m) devuelva los pares
-- de numeros parientes entre si, en el intervalo [n, m].
-- Por ejemplo,
-- parientes 1 30 ==
-- [(4,9),(6,6),(10,8),(14,10),(16,15),(18,21),(20,22),(22,14),(25,6)]
--

parientes = undefined

-- ---------------------------------------------------------------------------
-- EJERCICIO 4.   COMPRENSION / RECURSION
-- Representamos mediante una lista de pares los datos de las ventas de
-- una tienda de electrodomesticos. Por ejemplo: 
-- [
--  ("Lavadora"    , [("AEG", 20), ("Fagor",15),("Boch",5)]),
--  ("Lavavajillas", [("AEG",12),  ("Boch",8),  ("Zanussi",7), ("Fagor",4)])
--  ("Frigorifico" , [("Fagor",5), ("AEG",9),   ("Boch",3)]),
--  ("Lavadora"    , [("AEG", 2), ("Miele",5)]),
--  ]
--
-- EJERCICIO 4.1.
-- Definir una funcion ventasC, tal que (ventasC x xs) devuelva el numero
-- total de electrodomesticos tipo x vendidos segun la lista xs.
-- Por ejemplo, llamemos lista a la lista anterior:

lista :: [(String, [(String, Int)])]
lista = [("Lavavadora"    , [("AEG", 20), ("Fagor",15),("Boch",5)]),
    ("Lavavajillas", [("AEG",12),  ("Boch",8),  ("Zanussi",7), ("Fagor",4)]),
    ("Frigorifico" , [("Fagor",5), ("AEG",9),   ("Boch",3)]),
    ("Lavadora"    , [("AEG", 2), ("Miele",5)])]

-- ventasC "Lavavajillas" lista == 31 
-- Ya que 31 == 12+8+7+4
-- ventasC "Lavavadora" lista == 47
-- Ya que 47 == 20+15+5+2+5

ventasC :: String -> [(String, [(String, Int)])] -> Int
ventasC a ((x, y):xs) = ventasCAux (obtenerTipoElectrodomesticos a ((x, y):xs))

ventasCAux :: [(String, Int)] -> Int
ventasCAux xs = sum [y | (_, y) <- xs]

-- EJERCICIO 4.2.
-- Definir la version recursiva de la funcion anterior, ventasR.

ventasR :: String -> [(String, [(String, Int)])] -> Int
ventasR a ((x, y):xs) = ventasRAux (obtenerTipoElectrodomesticos a ((x, y):xs))

ventasRAux :: [(String, Int)] -> Int
ventasRAux ((x, y):xs) = y + ventasCAux xs

-- Función auxiliar:
obtenerTipoElectrodomesticos :: String -> [(String, [(String, Int)])] -> [(String, Int)]
obtenerTipoElectrodomesticos a ((x, y):xs)
    | a == x = y
    | otherwise = obtenerTipoElectrodomesticos a xs
obtenerTipoElectrodomesticos a [] = error "Tipo de electrodoméstico no encontrado."