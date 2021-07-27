-- PD-Práctica 3 
-- Definiciones por comprensión
-- =====================================================================

-- En esta relación se presentan ejercicios con definiciones de
-- funciones por comprensión. Estos ejercicios se corresponden con el
-- tema 5 
 
import Test.QuickCheck
import Data.Char

-- ---------------------------------------------------------------------
-- Funciones auxiliares
esPrimo :: Integral a => a -> Bool
esPrimo 2 = True
esPrimo n | n < 2 = False
    | even n = False
    | otherwise = n == (menorDivisor 3 n)

menorDivisor :: Integral a => a -> a -> a
menorDivisor k n | k * k > n = n
    | rem n k == 0 = k
    | otherwise = menorDivisor (k + 2) n

divisores :: Int -> [Int]
divisores n = [x | x <- [1..(n-1)], n `rem` x == 0]

toInt :: Integer -> Int 
toInt 0 = 0
toInt n = fromInteger n `mod` 2 + toInt (n `div` 2)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función
--    sumaDeCuadrados :: Integer -> Integer
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n números; es decir, 1^2 + 2^2 + ... + 100^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

sumaDeCuadrados :: Integer -> Integer
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un entero positivo es perfecto si es igual a la suma de
-- sus factores, excluyendo el propio número. Usando una lista por
-- comprensión y la función factores (del tema), definir la función 
--    perfectos :: Int -> [Int]
-- tal que (perfectos n) es la lista de todos los números perfectos
-- menores que n. Por ejemplo: 
--    *Main> perfectos 500
--    [6,28,496]
-- ---------------------------------------------------------------------
 
-- La función factores del tema es
factores :: Int -> [Int]
factores n = undefined

-- La definición es
perfectos :: Int -> [Int]
perfectos n = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 3. El producto escalar de dos listas de enteros xs y ys de
-- longitud n viene dado por la suma de los productos de los elementos
-- correspondientes. Definir por comprensión la función 
--    productoEscalar :: [Int] -> [Int] -> Int
-- tal que (productoEscalar xs ys) es el producto escalar de las listas
-- xs e ys. Por ejemplo,
--    productoEscalar [1,2,3] [4,5,6]  =>  32
--
-- Usar QuickCheck para comprobar la propiedad conmutativa del producto
-- escalar.  
-- ---------------------------------------------------------------------
 
-- La definición es
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar [] [] = 0
productoEscalar (x:xs) (y:ys) = x*y + productoEscalar xs ys

-- La propiedad conmutativa es
prop_conmutativa_productoEscalar xs ys = undefined

-- La comprobación es 

-- ---------------------------------------------------------------------
-- Ejercicio 4 (Problema 1 del proyecto Euler) Definir la función
--    euler1 :: Integer -> Integer
-- (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores que
-- n. Por ejemplo,
--    euler1 10  ==  23
-- 
-- Calcular la suma de todos los múltiplos de 3 ó 5 menores que 1000.
-- ---------------------------------------------------------------------

euler1 :: Integer -> Integer
euler1 n = undefined

-- El cálculo es

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir por comprensión la función
--    replica :: Int -> a -> [a]
-- tal que (replica n x) es la lista formada por n copias del elemento
-- x. Por ejemplo,
--    *Main> replica 3 True
--    [True, True, True]
-- Se corresponde con la función replicate.
-- ---------------------------------------------------------------------
 
replica :: Int -> a -> [a]
replica n x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Un número natural n se denomina abundante si es menor
-- que la suma de sus divisores propios. Por ejemplo, 12 y 30 son
-- abundantes pero 5 y 28 no lo son.
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función numeroAbundante tal que
-- (numeroAbundante n) se verifica si n es un número abundante. Por
-- ejemplo, 
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
-- ---------------------------------------------------------------------

numeroAbundante n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función numerosAbundantesMenores tal que
-- (numerosAbundantesMenores n) es la lista de números abundantes
-- menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

numerosAbundantesMenores n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función todosPares tal que (todosPares n)
-- se verifica si todos los números abundantes menores o iguales que n
-- son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

todosPares n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la constante primerAbundanteImpar que calcule
-- el primer número natural abundante impar. Determinar el valor de
-- dicho número.
-- ---------------------------------------------------------------------

primerAbundanteImpar = undefined

-- Su cálculo es

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función suma tal (suma n) es la suma de los
-- n primeros números. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

suma :: Integer -> Integer
suma n = sum [x | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Los triángulo aritmético se forman como sigue
--     1
--     2  3
--     4  5  6
--     7  8  9 10
--    11 12 13 14 15
--    16 17 18 19 20 21
-- Definir la función linea tal que (linea n) es la línea n-ésima de los
-- triángulos aritméticos. Por ejemplo, 
--    linea 4  ==  [7,8,9,10]
--    linea 5  ==  [11,12,13,14,15]
-- ---------------------------------------------------------------------

linea n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función triangulo tal que (triangulo n) es
-- el triángulo aritmético de altura n. Por ejemplo,
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

triangulo n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función circulo tal que (circulo n) es la
-- cantidad de pares de números naturales (x,y) que se encuentran dentro
-- del círculo de radio n. Por ejemplo, 
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
-- La ecuación de los puntos que están en la circunferencia de centro (0,0)
--  y radio n es: x^2 + y^2 = n^2. Los puntos del interior cumplen la 
-- desigualdad: x^2 + y^2 < n^2 
-- ---------------------------------------------------------------------

circulo n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función ocurrenciasDelMaximo tal que
-- (ocurrenciasDelMaximo xs) es el par formado por el mayor de los
-- números de xs y el número de veces que este aparece en la lista
-- xs, si la lista es no vacía y es (0,0) si xs es la lista vacía. Por
-- ejemplo,  
--    ocurrenciasDelMaximo [1,3,2,4,2,5,3,6,3,2,1,8,7,6,5]  ==  (8,1)
--    ocurrenciasDelMaximo [1,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,3)
--    ocurrenciasDelMaximo [8,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,4)
-- ---------------------------------------------------------------------

ocurrenciasDelMaximo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, por comprensión, la función tienenS tal que
-- (tienenS xss) es la lista de las longitudes de las cadenas de xss que
-- contienen el caracter 's' en mayúsculas o minúsculas. Por ejemplo, 
--    tienenS ["Este","es","un","examen","de","hoy","Suerte"]  ==  [4,2,6]
--    tienenS ["Este"]                                         ==  [4]
--    tienenS []                                               ==  []
--    tienenS [" "]                                            ==  []
-- ---------------------------------------------------------------------

tienenS :: [[Char]] -> [Int]
tienenS xss = filter (/= 0) [if tienenSAux x /= 0 then (length x) else 0 | x <- xss]

tienenSAux :: [Char] -> Integer
tienenSAux xs = sum [if x == 's' || x == 'S' then 1 else 0 | x <- xs] 

-- ---------------------------------------------------------------------
-- Ejercicio 13. Decimos que una lista está algo ordenada si para todo
-- par de elementos consecutivos se cumple que el primero es menor o
-- igual que el doble del segundo. Definir, por comprensión, la función
-- (algoOrdenada xs) que se verifica si la lista xs está algo ordenada. 
-- Por ejemplo, 
--    algoOrdenada [1,3,2,5,3,8]  ==  True
--    algoOrdenada [3,1]          ==  False
-- ---------------------------------------------------------------------

algoOrdenada xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensión, la función tripletas tal 
-- que (tripletas xs) es la listas de tripletas de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    tripletas [8,7,6,5,4] == [[8,7,6],[7,6,5],[6,5,4]]
--    tripletas "abcd"      == ["abc","bcd"]
--    tripletas [2,4,3]     == [[2,4,3]]
--    tripletas [2,4]       == []
-- ---------------------------------------------------------------------

tripletas xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función tresConsecutivas tal que
-- (tresConsecutivas x ys) se verifica si x ocurre tres veces seguidas
-- en la lista ys. Por ejemplo,
--    tresConsecutivas 3 [1,4,2,3,3,4,3,5,3,4,6]  ==  False
--    tresConsecutivas 'a' "abcaaadfg"            ==  True
-- ---------------------------------------------------------------------

tresConsecutivas x ys = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.1. Definir la función unitarios tal (unitarios n) es
-- la lista de números [n,nn, nnn, ....]. Por ejemplo. 
--    take 7 (unitarios 3) == [3,33,333,3333,33333,333333,3333333]
--    take 3 (unitarios 1) == [1,11,111]
-- ---------------------------------------------------------------------

unitarios x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función multiplosUnitarios tal que
-- (multiplosUnitarios x y n) es la lista de los n primeros múltiplos de
-- x cuyo único dígito es y. Por ejemplo,
--    multiplosUnitarios 7 1 2  == [111111,111111111111]
--    multiplosUnitarios 11 3 5 == [33,3333,333333,33333333,3333333333]
-- ---------------------------------------------------------------------

multiplosUnitarios x y n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función primosEntre tal que (primosEntre x y)
-- es la lista de los número primos entre x e y (ambos inclusive). Por
-- ejemplo, 
--    primosEntre 11 44  ==  [11,13,17,19,23,29,31,37,41,43]
-- ---------------------------------------------------------------------

primosEntre x y = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función cortas tal que (cortas xs) es la
-- lista de las palabras más cortas (es decir, de menor longitud) de la
-- lista xs. Por ejemplo,
--    ghci> cortas ["hoy", "es", "un", "buen", "dia", "de", "sol"]
--    ["es","un","de"]
-- ---------------------------------------------------------------------

cortas xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19. Un entero positivo n es libre de cuadrado si no es
-- divisible por ningún m^2 > 1. Por ejemplo, 10 es libre de cuadrado
-- (porque 10 = 2*5) y 12 no lo es (ya que es divisible por 2^2). 
-- Definir la función libresDeCuadrado tal que (libresDeCuadrado n) es
-- la lista de los primeros n números libres de cuadrado. Por ejemplo,
--    libresDeCuadrado 15  ==  [1,2,3,5,6,7,10,11,13,14,15,17,19,21,22]
-- ---------------------------------------------------------------------

libresDeCuadrado n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función masOcurrentes tal que
-- (masOcurrentes xs) es la lista de los elementos de xs que ocurren el
-- máximo número de veces. Por ejemplo,
--    masOcurrentes [1,2,3,4,3,2,3,1,4] == [3,3,3]
--    masOcurrentes [1,2,3,4,5,2,3,1,4] == [1,2,3,4,2,3,1,4]
--    masOcurrentes "Salamanca"         == "aaaa"
-- ---------------------------------------------------------------------

masOcurrentes xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 21.1. Definir la función numDiv tal que (numDiv x) es el
-- número de divisores del número natural x. Por ejemplo, 
--    numDiv 11 == 2 
--    numDiv 12 == 6 
-- ---------------------------------------------------------------------

numDiv x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 21.2. Definir la función entre tal que (entre a b c) es la
-- lista de los naturales entre a y b con, al menos, c divisores. Por
-- ejemplo,  
--    entre 11 16 5 == [12, 16]
-- ---------------------------------------------------------------------

entre a b c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función conPos tal que (conPos xs) es la
-- lista obtenida a partir de xs especificando las posiciones de sus
-- elementos. Por ejemplo, 
--    conPos [1,5,0,7] == [(1,0),(5,1),(0,2),(7,3)]
-- ---------------------------------------------------------------------

conPos xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función tal que (pares cs) es la cadena
-- formada por los caracteres en posición par de cs. Por ejemplo, 
--    pares "el cielo sobre berlin" == "e il or eln"
-- ---------------------------------------------------------------------

pares cs = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 24.1. Una terna (x,y,z) de enteros positivos es pitagórica
-- si x^2 + y^2 = z^2. Usando una lista por comprensión, definir la
-- función 
--    pitagoricas :: Int -> [(Int,Int,Int)]
-- tal que (pitagoricas n) es la lista de todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n. Por ejemplo, 
--    pitagoricas 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-- ---------------------------------------------------------------------

pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 24.2. Definir la función 
--    numeroDePares :: (Int,Int,Int) -> Int
-- tal que (numeroDePares t) es el número de elementos pares de la terna
-- t. Por ejemplo,
--    numeroDePares (3,5,7)  ==  0
--    numeroDePares (3,6,7)  ==  1
--    numeroDePares (3,6,4)  ==  2
--    numeroDePares (4,6,4)  ==  3
-- ---------------------------------------------------------------------

numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 24.3. Definir la función
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n tiene un número impar de números
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

conjetura :: Int -> Bool
conjetura n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir, por comprensión, la función
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 26. Los polinomios pueden representarse de forma dispersa o
-- densa. Por ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar
-- de forma dispersa por [6,0,-5,4,-7] y de forma densa por
-- [(4,6),(2,-5),(1,4),(0,-7)].  
-- 
-- Definir la función 
--    densa :: [Int] -> [(Int,Int)]
-- tal que, si xs es la forma dispersa de un polinomio, (densa xs) es la 
-- forma densa. 
-- representación dispersa es xs. Por ejemplo, 
--   densa [6,0,-5,4,-7]  ==  [(4,6),(2,-5),(1,4),(0,-7)]
--   densa [6,0,0,3,0,4]  ==  [(5,6),(2,3),(0,4)]
-- ---------------------------------------------------------------------

densa :: [Int] -> [(Int,Int)]
densa xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 27. La función 
--    pares2 :: [a] -> [b] -> [(a,b)]
-- definida por
--    pares2 xs ys = [(x,y) | x <- xs, y <- ys]
-- toma como argumento dos listas y devuelve la listas de los pares con
-- el primer elemento de la primera lista y el segundo de la
-- segunda. Por ejemplo,
--    ghci> pares2 [1..3] [4..6]
--    [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- 
-- Definir, usando dos listas por comprensión con un generador cada una,
-- la función 
--    pares2' :: [a] -> [b] -> [(a,b)]
-- tal que pares2' sea equivalente a pares2.
-- 
-- Indicación: Utilizar la función predefinida concat y encajar una
-- lista por comprensión dentro de la otra. 
-- ---------------------------------------------------------------------

-- La definición de pares es
pares2 :: [a] -> [b] -> [(a,b)]
pares2 xs ys = [(x,y) | x <- xs, y <- ys]

-- La redefinición de pares es
pares2' :: [a] -> [b] -> [(a,b)]
pares2' xs ys = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 28. La bases de datos sobre actividades de personas pueden
-- representarse mediante listas de elementos de la forma (a,b,c,d),
-- donde a es el nombre de la persona, b su actividad, c su fecha de
-- nacimiento y d la de su fallecimiento. Un ejemplo es la siguiente que
-- usaremos a lo largo de este ejercicio,
-- ---------------------------------------------------------------------

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]

-- ---------------------------------------------------------------------
-- Ejercicio 28.1. Definir la función nombres tal que (nombres bd) es
-- la lista de los nombres de las personas de la base de datos bd. Por
-- ejemplo,  
--    ghci> nombres personas
--     ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
--      "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]
-- ---------------------------------------------------------------------

nombres :: [(String,String,Int,Int)] -> [String]
nombres [] = []
nombres ((x, y, z, t):bd) = x : nombres bd

-- ---------------------------------------------------------------------
-- Ejercicio 28.2. Definir la función musicos tal que (musicos bd) es
-- la lista de los nombres de los músicos de la base de datos bd. Por
-- ejemplo,  
--    ghci> musicos personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos :: [(String,String,Int,Int)] -> [String]
musicos [] = []
musicos ((x, y, z, t):bd)
    | y == "Musica" = x : musicos bd
    | otherwise     = musicos bd

-- ---------------------------------------------------------------------
-- Ejercicio 28.3. Definir la función seleccion tal que (seleccion bd m) 
-- es la lista de los nombres de las personas de la base de datos bd
-- cuya actividad es m. Por ejemplo,  
--    ghci> seleccion personas "Pintura"
--    ["Velazquez","Picasso","Goya","Botticelli"]
-- ---------------------------------------------------------------------

seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion [] _ = []
seleccion ((x, y, z, t):bd) m 
    | y == m    = x : seleccion bd m
    | otherwise = seleccion bd m

-- ---------------------------------------------------------------------
-- Ejercicio 28.4. Definir, usando el apartado anterior, la función
-- musicos' tal que (musicos' bd) es la lista de los nombres de los
-- músicos de la base de datos bd. Por ejemplo,  
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

-- ---------------------------------------------------------------------
-- Ejercicio 28.5. Definir la función vivas tal que (vivas bd a) es la
-- lista de los nombres de las personas de la base de datos bd  que
-- estaban vivas en el año a. Por ejemplo,  
--    ghci> vivas personas 1600
--    ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas [] _ = []
vivas ((x, y, z, t):ps) a
    | t > a && z < a = x : vivas ps a
    | otherwise     = vivas ps a

-- ---------------------------------------------------------------------
-- Ejercicio 29.1. En este ejercicio se consideran listas de ternas de
-- la forma (nombre, edad, población). 
-- 
-- Definir la función puedenVotar tal que (puedenVotar t) es la
-- lista de las personas de t que tienen edad para votar. Por ejemplo,
--    ghci> :{
--    *Main| puedenVotar [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), 
--    *Main|              ("Alba", 19, "Camas"), ("Pedro",18,"Sevilla")]
--    *Main| :}
--    ["Juan","Alba","Pedro"]
-- ---------------------------------------------------------------------

-- Para que sea más cómodo, vamos a inicializar la población.
poblacion :: [(String, Integer, String)]
poblacion = [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), ("Alba", 19, "Camas"),("Pedro",18,"Sevilla")] 

puedenVotar :: [(String, Integer, String)] -> [String]
puedenVotar [] = []
puedenVotar ((x, y, z):t)
    | y >= 18   = x : puedenVotar t
    | otherwise = puedenVotar t

-- ---------------------------------------------------------------------
-- Ejercicio 29.2. Definir la función puedenVotarEn tal que (puedenVotar
-- t p) es la lista de las personas de t que pueden votar en la
-- población p. Por ejemplo, 
--    ghci> :{
--    *Main| puedenVotarEn [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), 
--    *Main|                ("Alba", 19, "Camas"),("Pedro",18,"Sevilla")] 
--    *Main|               "Sevilla"
--    *Main| :}
--    ["Pedro"]
-- ---------------------------------------------------------------------

puedenVotarEn :: [(String, Integer, String)] -> String -> [String]
puedenVotarEn [] _ = []
puedenVotarEn ((x, y, z):t) c
    | z == c && y >= 18 = x : puedenVotarEn t c
    | otherwise         = puedenVotarEn t c

-- ---------------------------------------------------------------------
-- Ejercicio 30. Dos listas xs, ys de la misma longitud son
-- perpendiculares si el producto escalar de ambas es 0, donde el
-- producto escalar de dos listas de enteros xs e ys viene
-- dado por la suma de los productos de los elementos correspondientes.
-- 
-- Definir la función perpendiculares tal que (perpendiculares xs yss)
-- es la lista de los elementos de yss que son perpendiculares a xs.
-- Por ejemplo,
--    ghci> perpendiculares [1,0,1] [[0,1,0], [2,3,1], [-1,7,1],[3,1,0]]
--    [[0,1,0],[-1,7,1]]
-- ---------------------------------------------------------------------

esPerpendicular :: [Integer] -> [Integer] -> Bool
esPerpendicular xs ys = sum [(xs !! x) * (ys !! x) | x <- [0..length xs-1]] == 0

perpendiculares :: [Integer] ->  [[Integer]] -> [[Integer]]
perpendiculares _ [] = []
perpendiculares xs (ys:yss)
    | esPerpendicular xs ys = ys : perpendiculares xs yss
    | otherwise             = perpendiculares xs yss

-- ---------------------------------------------------------------------
-- Ejercicio 31.1. Un número natural n
-- es especial si para todo divisor d de n, d+n/d es primo. Definir la
-- función  
--    especial :: Integer -> Bool
-- tal que (especial x) se verifica si x es especial. Por ejemplo,
--    especial 30  ==  True
--    especial 20  ==  False
-- ---------------------------------------------------------------------

especial :: Integer -> Bool
especial n = all (== True) [esPrimo (x + fromIntegral (div n (fromIntegral x))) | x <- divisores (bits n)]

-- ---------------------------------------------------------------------
-- Ejercicio 31.2. Definir la función 
--    sumaEspeciales :: Integer -> Integer
-- tal que (sumaEspeciales n) es la suma de los números especiales
-- menores o iguales que n. Por ejemplo, 
--    sumaEspeciales 100  ==  401
-- ---------------------------------------------------------------------

sumaEspeciales :: Integer -> Integer
sumaEspeciales n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 32.1. Un número es muy compuesto si tiene más divisores que
-- sus anteriores. Por ejemplo, 12 es muy compuesto porque tiene 6
-- divisores (1, 2, 3, 4, 6, 12) y todos los números del 1 al 11 tienen
-- menos de 6 divisores.  
-- 
-- Definir la función
--    esMuyCompuesto :: Int -> Bool
-- tal que (esMuyCompuesto x) se verifica si x es un número muy
-- compuesto. Por ejemplo,
--    esMuyCompuesto 24  ==  True
--    esMuyCompuesto 25  ==  False
-- Calcular  el menor número muy compuesto de 4 cifras.
-- ---------------------------------------------------------------------

esMuyCompuesto :: Int -> Bool
esMuyCompuesto x = esMuyCompuestoAux (divisores x) (length (divisores x))

esMuyCompuestoAux :: [Int] -> Int -> Bool
esMuyCompuestoAux [] _ = True
esMuyCompuestoAux (x:xs) y
    | length (divisores x) < fromIntegral y = True && esMuyCompuestoAux xs y
    | otherwise = False

-- ---------------------------------------------------------------------
-- Ejercicio 32.2. Definir la función
--    muyCompuesto :: Int -> Int
-- tal que (muyCompuesto n) es el n-ésimo número muy compuesto. Por
-- ejemplo, 
--    muyCompuesto 10  ==  180
-- ---------------------------------------------------------------------

muyCompuesto :: Int -> Int
muyCompuesto n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la función  
--     todosIguales :: Eq a => [a] -> Bool
-- tal que (todosIguales xs) se verifica si los elementos de la 
-- lista xs son todos iguales. Por ejemplo,   
--     todosIguales [1..5]    == False
--     todosIguales [2,2,2]   == True
--     todosIguales ["a","a"] == True
-- ---------------------------------------------------------------------

todosIguales:: Eq a => [a] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) 
    | x == y    = True && todosIguales (y:xs)
    | otherwise = False

-- ---------------------------------------------------------------------
-- Ejercicio 34.1. Las bases de datos de alumnos matriculados por
-- provincia y por especialidad se pueden representar como sigue 
--    matriculas :: [(String,String,Int)]
--    matriculas = [("Almeria","Matematicas",27),
--                  ("Sevilla","Informatica",325),
--                  ("Granada","Informatica",296),
--                  ("Huelva","Matematicas",41),
--                  ("Sevilla","Matematicas",122),
--                  ("Granada","Matematicas",131),
--                  ("Malaga","Informatica",314)]
-- Es decir, se indica que por ejemplo en Sevilla hay 325 alumnos
-- matriculados en Informática. 
-- 
-- Definir la función 
--    totalAlumnos :: [(String,String,Int)] -> Int
-- tal que (totalAlumnos bd) es el total de alumnos matriculados,
-- incluyendo todas las provincias y todas las especialidades, en la
-- base de datos bd. Por ejemplo,
--    totalAlumnos matriculas == 1256
-- ---------------------------------------------------------------------

matriculas :: [(String,String,Int)]
matriculas = [("Almeria","Matematicas",27),
              ("Sevilla","Informatica",325),
              ("Granada","Informatica",296),
              ("Huelva","Matematicas",41),
              ("Sevilla","Matematicas",122),
              ("Granada","Matematicas",131),
              ("Malaga","Informatica",314)]

totalAlumnos :: [(String,String,Int)] -> Int
totalAlumnos [] = 0
totalAlumnos ((x, y, z):xs) = z + totalAlumnos xs

-- ---------------------------------------------------------------------
-- Ejercicio 34.2. Definir la función 
--    totalMateria :: [(String,String,Int)] -> String -> Int
-- tal que (totalMateria bd m) es el número de alumnos de la base de
-- datos bd matriculados en la materia m. Por ejemplo, 
--    totalMateria matriculas "Informatica" == 935
--    totalMateria matriculas "Matematicas" == 321
--    totalMateria matriculas "Fisica"      == 0
-- ---------------------------------------------------------------------

totalMateria :: [(String,String,Int)] -> String -> Int
totalMateria [] _ = 0
totalMateria ((x, y, z):xs) m 
    | m == y    = z + totalMateria xs m
    | otherwise = totalMateria xs m

-- ---------------------------------------------------------------------
-- Ejercicio 35. Dada una lista de números enteros, definiremos el
-- mayor salto como el mayor valor de las diferencias (en valor
-- absoluto) entre números consecutivos de la lista. Por ejemplo, dada
-- la lista [2,5,-3] las distancias son 
--    3 (valor absoluto de la resta 2 - 5) y
--    8 (valor absoluto de la resta de 5 y (-3))
-- Por tanto, su mayor salto es 8. No está definido el mayor salto para
-- listas con menos de 2 elementos
--
-- Definir la función 
--    mayorSalto :: [Integer] -> Integer
-- tal que (mayorSalto xs) es el mayor salto de la lista xs. Por
-- ejemplo, 
--    mayorSalto [1,5]              == 4
--    mayorSalto [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------

valorAbsoluto :: Integer -> Integer
valorAbsoluto n
    | n >= 0 = n
    | otherwise = (-n)

mayorSalto :: [Integer] -> Integer
mayorSalto (x:y:xs) = mayorSaltoAux (y:xs) x

mayorSaltoAux :: [Integer] -> Integer -> Integer
mayorSaltoAux [] z = z  
mayorSaltoAux (x:y:xs) z 
    | (x-y) > (z-x)     = mayorSaltoAux (y:xs) (x-y)
    | otherwise = mayorSaltoAux (y:xs) (z-x)

