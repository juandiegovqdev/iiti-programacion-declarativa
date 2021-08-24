-- PD-Practica 6.1
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Redefinir por recursión la función
--    takeWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhile p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhile' (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------
 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Redefinir por recursión la función
--    dropWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhile p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que no cumple la propiedad p. Por ejemplo,
--    dropWhile' (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------
 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = x:xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 3.1 Redefinir, usando foldr, la función concat. Por ejemplo, 
--    concat' [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------
 
concat' :: [[a]] -> [a]
concat' = foldr (\xs ys -> xs ++ ys) []

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la funciones concat',
-- y concat son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss =
  concat' xss == concat xss

-- La comprobación es
--    ghci> quickCheck prop_concat
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que la longitud de 
-- (concat' xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss =
    length (concat' xss) == sum [length xs | xs <- xss]

-- La comprobación es
--    ghci> quickCheck prop_longConcat
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [a]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos _ [] = []
segmentos p (x:xs) 
    | p x       = takeWhile p (x:xs) : segmentos p (dropWhile p xs)
    | otherwise = segmentos p xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. La función 
--    divideMedia :: [Double] -> ([Double],[Double])
-- dada una lista numérica, xs, calcula el par (ys,zs), donde ys 
-- contiene los elementos de xs estrictamente menores que la media, 
-- mientras que zs contiene los elementos de xs estrictamente mayores 
-- que la media. Por ejemplo, 
--    divideMedia [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMedia [1,2,3]         ==  ([1.0],[3.0])
-- Definir la función divideMedia por filtrado y por recursión. 
-- ---------------------------------------------------------------------
 
-- La definición por filtrado es
divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF xs = (menores,mayores)
  where menores = filter (<media) xs
        mayores = filter (>media) xs
        media = sum xs / fromIntegral (length xs)
 
-- La definición por recursión es
divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR xs = (menores m xs,mayores m xs)
  where menores m [] = []
        menores m (x:xs)
          | x<m = x : menores m xs
          | otherwise = menores m xs
        mayores m [] = []
        mayores m (x:xs)
          | x>m = x : mayores m xs
          | otherwise = mayores m xs
        m = sum xs / fromIntegral (length xs)
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por
-- ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------
 
agrupa :: Eq a => [[a]] -> [[a]]
agrupa []  = []
agrupa xss
    | [] `elem` xss = []
    | otherwise     = primeros xss : agrupa (restos xss)
    where primeros = map head
          restos   = map tail
 
-- ---------------------------------------------------------------------
-- Ejercicio 7. Se considera la función 
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  =>  [5,6]
-- Se pide, definir la función
-- 1. por comprensión,
-- 2. usando map y filter,
-- 3. por recursión y
-- 4. por plegado (con foldr).
-- ---------------------------------------------------------------------
 
-- La definición con lista de comprensión es
filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x <- xs, p x]
 
-- La definición con map y filter es
filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaMF f p xs = map f (filter p xs)
 
-- La definición por recursión es
filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR _ _ [] = []
filtraAplicaR f p (x:xs) | p x       = f x : filtraAplicaR f p xs
                         | otherwise = filtraAplicaR f p xs
 
-- La definición por plegado es
filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4 f p = foldr (\x ys -> if p x then f x:ys else ys) []
 
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir, usando recursión, plegado, y recursión con
-- acumulador la  función
--    inversa :: [a] -> [a]
-- tal que (inversa xs) es la inversa de la lista xs. Por ejemplo,
--    inversa [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaR :: [a] -> [a]
inversaR [] = []
inversaR (x:xs) = inversaR xs ++ [x]

inversaP :: [a] -> [a]
inversaP = foldr (\x ys -> ys ++ [x]) []

inversaAC :: [a] -> [a]
inversaAC xs = aux [] xs
  where aux ys [] = ys
        aux ys (x:xs) = aux (x:ys) xs

-- ---------------------------------------------------------------------
-- Ejercicio 9. La función de plegado foldl está definida por
--    foldl :: (a -> b -> a) -> a -> [b] -> a
--    foldl f ys xs = aux ys xs
--        where aux ys []     = ys
--              aux ys (x:xs) = aux (f ys x) xs
-- Definir, mediante plegado con foldl, la función
--    inversaP' :: [a] -> [a]
-- tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--    inversaP' [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaP' :: [a] -> [a]
inversaP' = foldl (\ys x -> x:ys) []

-- ---------------------------------------------------------------------
-- Ejercicio 10. Redefinir, por recursión y plegado la función map. 
-- ---------------------------------------------------------------------

mapR :: (a -> b) -> [a] -> [b]
mapR _ [] = []
mapR f (x:xs) = f x : mapR f xs

mapP :: (a -> b) -> [a] -> [b]
mapP f = foldr (\x y -> f x : y) []

-- ---------------------------------------------------------------------
-- Ejercicio 11. Redefinir, usando foldl y foldr la función filter. Por
-- ejemplo, 
--    filter (<4) [1,7,3,2]  =>  [1,3,2]
-- ---------------------------------------------------------------------

filterL :: (a -> Bool) -> [a] -> [a]
filterL p = foldl (\ys x -> if p x then ys++[x] else ys) []

filterR :: (a -> Bool) -> [a] -> [a]
filterR p = foldr (\x ys -> if p x then (x:ys) else ys) []

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, mediante recursión, plegado, acumulador, y 
-- plegado con foldl la función
--    sumll :: Num a => [[a]] -> a
-- tal que (sumll xss) es la suma de las sumas de las listas de xss. 
-- Por ejemplo, 
--    sumll [[1,3],[2,5]]  ==  11
-- ---------------------------------------------------------------------

sumllR :: Num a => [[a]] -> a
sumllR [] = 0
sumllR (xs:xss) = sum xs + sumllR xss

sumllP :: Num a => [[a]] -> a
sumllP = foldr (\x ys -> sum x + ys) 0

sumllA :: Num a => [[a]] -> a
sumllA xs = aux 0 xs
  where aux acc [] = acc
        aux acc (xs:xss) = aux (acc+sum xs) xss

sumllAP :: Num a => [[a]] -> a
sumllAP = foldl (\y x -> y+sum x) 0

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir, mediante recursión y plegado, la función
--    borra :: Eq a => a -> a -> [a]
-- tal que (borra y xs) es la lista obtenida borrando las ocurrencias de
-- y en xs. Por ejemplo, 
--    borra 5 [2,3,5,6]    ==  [2,3,6]
--    borra 5 [2,3,5,6,5]  ==  [2,3,6]
--    borra 7 [2,3,5,6,5]  ==  [2,3,5,6,5]
-- ---------------------------------------------------------------------

borraR :: Eq a => a -> [a] -> [a]
borraR _ [] = []
borraR y (x:xs)
  | y==x = borraR y xs
  | otherwise = x:borraR y xs

borraP :: Eq a => a -> [a] -> [a]
borraP z = foldr (\x ys -> if x==z then ys else x:ys) [] 

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, mediante recursión y plegado la función
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia del conjunto xs e ys; es
-- decir el conjunto de los elementos de xs que no pertenecen a ys. Por
-- ejemplo,  
--    diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
-- ---------------------------------------------------------------------

diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR [] _ = []
diferenciaR xs [] = xs
diferenciaR (x:xs) ys
  | elem x ys = diferenciaR xs ys
  | otherwise = x : diferenciaR xs ys

diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP xs ys = foldr (\x zs -> if notElem x ys then x:zs else zs) [] xs

-- -------------------------------------------------------------------
-- Ejercicio 15. Definir mediante plegado la función 
--    producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los elementos de la lista
-- xs. Por ejemplo, 
--    producto [2,1,-3,4,5,-6] == 720
-- ---------------------------------------------------------------------

producto :: Num a => [a] -> a
producto = foldr (*) 1

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir mediante plegado la función 
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (productoPred p xs) es el producto de los elementos de la
-- lista xs que verifican el predicado p. Por ejemplo, 
--    productoPred even [2,1,-3,4,-5,6] == 48
-- ---------------------------------------------------------------------

productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred p = foldl (\acc x -> if p x then x*acc else acc) 1

-- ---------------------------------------------------------------------
-- Ejercicio 17.1. Definir, mediante recursión, la función
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumR :: Ord a => [a] -> a
maximumR [x]      = x
maximumR (x:y:ys) = max x (maximumR (y:ys))
maximumR _        = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 17.2. La función de plegado foldr1 está definida por 
--    foldr1 :: (a -> a -> a) -> [a] -> a
--    foldr1 _ [x]    =  x
--    foldr1 f (x:xs) =  f x (foldr1 f xs)
-- 
-- Definir, mediante plegado con foldr1, la función
--    maximumP :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumP [3,7,2,5]                  ==  7
--    maximumP ["todo","es","falso"]      ==  "todo"
--    maximumP ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumP es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumP :: Ord a => [a] -> a
maximumP = foldr1 max

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir, por plegado, la función
-- sumaDivP :: Int -> [Int] -> Int
-- tal que (SumaDivP x xs) es la suma de los cuadrados de los
-- elementos de xs que son divisibles por x. Por ejemplo,
-- sumaDivP 3 [1..7] == 45
-- sumaDivP 2 [1..7] == 56
-- ---------------------------------------------------------------------
sumaDivP :: Int -> [Int] -> Int
sumaDivP x = foldr (\y z -> if rem y x == 0 then y^2+z else z) 0

-- ---------------------------------------------------------------------
-- Ejercicio 19.1. Definir, con la función all, la función
--    relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosA r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosA (<) [2,3,7,9]                ==  True
--    relacionadosA (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- Se redefine la relación 'r' con 'rpar' para que se aplique a pares
relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
relacionadosA r xs = all rpar (zip xs (tail xs))
  where rpar (x,y) = r x y

-- la función uncurry pasa una función f x y a f (x,y)
relacionadosA' :: (a -> a -> Bool) -> [a] -> Bool
relacionadosA' r xs = all (uncurry r) (zip xs (tail xs))

-- ---------------------------------------------------------------------
-- Ejercicio 19.2. Definir, con la función foldr, la función
--    relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosP r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosP (<) [2,3,7,9]                ==  True
--    relacionadosP (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
relacionadosP r xs = foldr rfpar True (zip xs (tail xs))
  where rfpar (x,y) b = (r x y) && b

-- ---------------------------------------------------------------------
-- Ejercicio 19.3. (Basado en el ejercicio 4 del primer parcial)
-- Una lista se dirá muy creciente si cada elemento es mayor estricto
-- que el triple del siguiente. 
-- Empleando tan solo (relacionadosA p xs), define el predicado 
--          muyCreciente :: [Integer] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es muy creciente. Por
-- ejemplo:
-- muyCreciente [1,5,23,115]  == True
-- muyCreciente [1,2,7,14]    == False
-- muyCreciente [7]           == True
-- muyCreciente []            == True
-- ---------------------------------------------------------------------

muyCreciente :: [Integer] -> Bool
muyCreciente xs = relacionadosA relMuyCreciente xs
  where relMuyCreciente a b = b > a*3
