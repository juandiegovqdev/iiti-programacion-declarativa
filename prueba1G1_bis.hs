-- ---------------------------------------------------------------------------
-- PD. Grado en Informatica. Tecnologias Informaticas. CURSO 2013-14
-- PRUEBA 1 DE EVALUACION ALTERNATIVA (6 NOVIEMBRE 2013)
-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------
-- NOMBRE Y APELLIDOS:
-- GRUPO:
-- EMAIL:
-- ---------------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- EJERCICIO 1. COMPRENSION
-- Una lista de numeros es mitica si la diferencia en valor absoluto de
-- cualesquiera dos elementos consecutivos es siempre menor o igual que la
-- posicion del segundo de ellos. Por ejemplo,
-- mitica [1,1,2,-1] == True
-- Ya que 1-1 = 0 <= 1 = posicion del 1, 
-- |1-2|=1 <= 2 posicion del 2,
-- |2-(-1)|= 3 <= 3 posicion del -1.
-- mitica [1,3,0,1,2] == False
-- Ya que |1-3| = 2 no es <= 1, que es la posicion del 3.
-- Definir por comprension una funcion (mitica xs) tal que reconozca  si
-- xs es una lista mitica.
-- SOLUCION:
mitica :: [Int] -> Bool
mitica = undefined

-- ---------------------------------------------------------------------------
-- EJERCICIO 2. USANDO COMPRENSION / USANDO RECURSION
-- Representamos un polinomio como la lista de los pares 
-- [(n_1, a_1), ... ,(n_m,a_m)]  donde en cada par, el a_i es el
-- coeficiente del monomio y n_i el grado. 
-- Por ejemplo, el polinomio 
-- 3x^5 - 2x^3 - x - 2 se representara por
-- [(5,3),(3,-2),(1,-1),(0,-2)]
--
-- EJERCICIO 2.1.
-- Definir usando recursion la funcion sustitR tal que 
-- (sustitR ps x) devuelve el valor de sustituir x en el 
-- polinomio ps. Definir una funcion esRaizR tal que (esRaizR ps z)
-- detecte cuando un  numero z es raiz de un polinomio ps.
-- Por ejemplo:
-- sustitR [(5,3),(3,2),(1,-1)] 0 == 0
-- sustitR [(2,2),(1,-2),(0,1)] 1 == 1
-- sustitR [(2,2),(1,-2),(0,1)] (-1) == 5
-- SOLUCION:
sustitR :: [(Float, Float)] -> Float  -> Float
sustitR = undefined

esRaizR = undefined

-- EJERCICIO 2.2.
-- Definir la funcion sustitC, que actue como sustitR, usando
-- comprension. Definir una funcion esRaizC tal que (esRaizC ps z)
-- detecte cuando un  numero z es raiz de un polinomio ps.
-- SOLUCION:
sustitC :: [(Float, Float)] -> Float -> Float
sustitC = undefined

esRaizC = undefined

-- EJERCICIO 2.3.
-- Definir una propiedad para comprobar con quickCheck que esRaizR y
-- esRaizC coinciden.
-- SOLUCION:
prop_raiz = undefined 

-- ---------------------------------------------------------------------------
-- EJERCICIO 3.   COMPRENSION
-- Dos numeros positivos, no primos a y b, son lejanos si la
-- suma de los divisores propios de a coincide con el producto de los
-- divisores propios de b. Por ejemplo, 25 y 6 son lejanos porque los
-- divisores propios de 25 son 1,5; los divisores propios de 6 son 1,2 y
-- 3 y  la suma 1+5 = 1*2*3. Sin embargo, 6 y 25 no son lejanos, ya que
-- 1+2+3 no es igual a 1*5.
--
-- EJERCICIO 3.1.
-- Define la funcion sonLejanos, tal que (sonLejanos x y) compruebe
-- si dos numeros cualesquiera x e y son lejanos. Por ejemplo:
-- sonLejanos 8 49 == True
-- sonLejanos 25 6 == True
-- sonLejanos 6 25 == False
--
-- SOLUCION:
sonLejanos = undefined

-- EJERCICIO 3.2.
-- Define la funcion lejanos, tal que (lejanos n) devuelva los pares
-- de numeros lejanos entre si, menores o iguales que n.
-- Por ejemplo,
-- lejanos 33 ==
-- [(4,9),(6,6),(10,8),(14,10),(16,15),(18,21),(20,22),
--  (22,14),(25,6),(33,15)]
--
-- SOLUCION:
lejanos = undefined

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
-- Definir una funcion vTotal, tal que (vTotal xs) devuelva la lista
-- total de electrodomesticos vendidos segun la lista xs,
-- independientemente del tipo.
-- Por ejemplo, llamemos lista a la lista anterior:
--
lista :: [(String, [(String, Int)])]
lista = [("Lavadora"    , [("AEG", 20), ("Fagor",15),("Boch",5)]),
    ("Lavavajillas", [("AEG",12),  ("Boch",8),  ("Zanussi",7), ("Fagor",4)]),
    ("Frigorifico" , [("Fagor",5), ("AEG",9),   ("Boch",3)]),
    ("Lavadora"    , [("AEG", 2), ("Miele",5)])]
--
-- vTotal  lista == 
-- [("AEG",20),("Fagor",15),("Boch",5),("AEG",12),
--  ("Boch",8),("Zanussi",7),("Fagor",4),("Fagor",5),
--  ("AEG",9),("Boch",3),("AEG",2),("Miele",5)]
-- 
-- SOLUCION:
vTotal :: [(String, [(String, Int)])] -> [(String, Int)]
vTotal = undefined

--
-- EJERCICIO 4.2.
-- Definir por comprension y recursion las funciones ventasC y
-- ventasR. De manera que (ventasC st l) y (ventasR st l) calculen el
-- numero total de electrodomesticos de la marca st vendidos segun la
-- lista de ventas totales l.
-- Por ejemplo, 
--  ventasR "Fagor" (vTotal lista) == 24
--  ventasR "Miele" (vTotal lista) == 5
--  ventasR "HP" (vTotal lista) == 0
-- SOLUCION:

ventasC :: String ->  [(String, Int)] -> Int
ventasC = undefined


ventasR :: String ->  [(String, Int)] -> Int
ventasR   = undefined