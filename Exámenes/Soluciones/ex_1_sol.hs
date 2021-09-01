-- --------------------------------------------------------------------
-- Definir la función
--   tieneRepeticiones :: Eq a => [a] -> Bool
-- tal que (tieneRepeticiones xs) se verifica si xs tiene algun elemento
-- repetido. Por ejemplo, 
-- tieneRepeticiones [3,2,5,2,7]          ==  True
   -- tieneRepeticiones [3,2,5,4,7]          ==  False
   -- tieneRepeticiones (5:[1..2000000000])  ==  True
   -- tieneRepeticiones [1..20000]           ==  False
-- SOLUCION:
tieneRepeticiones [] = False
tieneRepeticiones (x:xs) = elem x xs || tieneRepeticiones xs
-- --------------------------------------------------------------------
-- Definir la funci�n
-- particion :: [a] -> [Int] -> [[a]]
-- tal que (particion xs ns) es la partici�n de xs donde la longitud de
-- cada parte est� determinada por los elementos de ns. Por ejemplo, 
--
-- particion [1..10] [2,5,0,3]  ==  [[1,2],[3,4,5,6,7],[],[8,9,10]] 
-- particion [1..10] [1,4,2,3]  ==  [[1],[2,3,4,5],[6,7],[8,9,10]]

-- Usando recursión:
particion [] _     = []
particion _ []     = []
particion xs (n:ns) = take n xs : particion (drop n xs) ns

-- --------------------------------------------------------------------
-- Define la funcion
-- sinRepe :: [a] -> [a]
-- tal que (sinRepe xs) devuelve la lista xs sin repeticiones, da igual
-- el orden. Por ejemplo,
-- sinRepe [1,1,1,2] = [1,2]
-- sinRepe [1,1,2,1,1] = [2,1]
-- sinRepe [1,2,4,3,4,2,5,3,4,2] = [1,5,3,4,2]

sinRepe [] = []
sinRepe (x:xs) = if elem x xs then sinRepe xs else x:(sinRepe xs)

-- --------------------------------------------------------------------
-- Definir, POR RECURSION Y UTILIZANDO FUNCIONES DE ORDEN SUPERIOR, 
-- la funcion 
--    alguno :: (a -> Bool) -> [[a]] -> Bool
-- tal  que (alguno p xs) se verifica si cada elemento de la lista xss
-- contiene algun elemento que cumple el predicado p. Por ejemplo,
--    alguno odd [[1,3,4,2], [4,5], [9]] == True
--    alguno odd [[1,3,4,2], [4,8], [9]] == False
-- 
-- COMPRENSION-ORDEN SUPERIOR
alguno :: (a -> Bool) -> [[a]] -> Bool
alguno p xss = and [any p xs|xs<-xss]
--
alguno1 p xss = and [not(null [x|x<-xs, p x])|xs<-xss]
alguno2 p xss = and [not(null (filter p xs)) |xs<-xss]
-- RECURSION
alg p [] = True
alg p (xs:xss) = not(null [x|x<-xs, p x]) && alg p xss
--
alg1 p [] = True
alg1 p (xs:xss) = length (filter p xs) >=1 && alg1 p xss

-- --------------------------------------------------------------------
-- Definir USANDO FUNCIONES DE ORDEN SUPERIOR Y POR RECURSION, 
-- una funcion 
-- quitaPri :: (a -> Bool) -> [a] -> [a]
-- tal que (quitaPri p xs) devuelva la lista que resulta de eliminar de
-- xs el primer elemento que no cumpla p. Por ejemplo,
--  quitaPri (<2) [1,2,3,4] == [1,3,4]
--  quitaPri (>2) [1,2,3,4] == [2,3,4]
--  quitaPri ( `elem` "ca") "calamar" == "caamar"
--  quitaPri (/='m') "calamar" == "calaar"
-- SOLUCION:
-- ORDENSUPERIOR
quitaPri p xs = takeWhile p xs ++ tail (dropWhile p xs)
-- RECURSION
quitaP p [] = []
quitaP p (x:xs) | p x = x:quitaP p  xs
                | otherwise =  xs
