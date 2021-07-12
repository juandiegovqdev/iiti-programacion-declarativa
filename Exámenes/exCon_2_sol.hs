-- ------------------------------------------------------------------
-- PD.SEGUNDA CONVOCATORIA. CURSO 14-15
-- 2 DE SEPTIEMBRE 2015
-- ------------------------------------------------------------------
-- NOMBRE Y APELLIDOS:
-- UVUS:
-- ------------------------------------------------------------------
--
-- Antes de comenzar, renombra estefichero: uvus_sep.hs
--
-- --------------------------------------------------------------------
-- ORDEN SUPERIOR (FILTER, MAP, ALL, etc.) 1PUNTO
-- Definir la funcion 
--    verificaP :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaP p xs) se verifica si cada elemento de la lista xss
-- contiene algun elemento que cumple el predicado p. Por ejemplo,
--    verificaP odd [[1,3,4,2], [4,5], [9]] == True
--    verificaP odd [[1,3,4,2], [4,8], [9]] == False

-- SOLUCIONES
-- 1ª definicion (por comprension):
verificaP :: (a -> Bool) -> [[a]] -> Bool
verificaP p xss = and [any p xs | xs <- xss]
 
-- 2ª definicion (por recursion):
verificaP2 :: (a -> Bool) -> [[a]] -> Bool
verificaP2 p []       = True
verificaP2 p (xs:xss) = any p xs && verificaP2 p xss
 
-- 3ª definicion (por plegado):
verificaP3 :: (a -> Bool) -> [[a]] -> Bool
verificaP3 p = foldr ((&&) . any p) True
 
-- --------------------------------------------------------------------
-- ARBOLES 1,5PUNTOS
-- Consideramos el siguiente tipo algebraico de los arboles binarios:

data Arbol a = H a
                | N a (Arbol a) (Arbol a)
                  deriving (Show,Eq)

-- y el siguiente arbol

a1 :: Arbol Int
a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)

-- Diremos que una propiedad es AF en un arbol si se cumple en alg�n 
-- nodo de cada posible rama del arbol (una rama es la  secuencia de 
-- nodos  desde el nodo inicial o raiz hasta una hoja). 
-- En este arbol a1 se cumple (AF par); es decir, en todas las ramas hay un
-- numero par;  pero no se cumple (AF primo); es decir, hay ramas en las
-- que  no hay ningun numero primo.
-- Definir la funcion
-- propiedadAF :: (a -> Bool) -> Arbol a -> Bool
-- tal que (propiedadAF p a) se verifica si se cumple (AF p) en el arbol
-- a; es  decir, si en todas las ramas hay algun nodo (interno u hoja)
-- que cumple  la propiedad p. Por ejemplo:
-- propiedadAF even a1  ==  True
-- propiedadAF (<7) a1  ==  False
-- SOLUCION:
propiedadAF :: (a -> Bool) -> Arbol a -> Bool
propiedadAF p (H a)     = p a
propiedadAF p (N a i d) = p a || (propiedadAF p i && propiedadAF p d)

-- --------------------------------------------------------------------
-- -- ARBOLES 1,5PUNTOS
-- Representamos los arboles binarios con elementos en las hojas y en
-- los nodos mediante el mismo tipo de dato que el ejercicio anterior:
-- data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
-- Por ejemplo,
ej1 :: Arbol Int
ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))

-- Definir la funcion
-- ramasCon :: Eq a => Arbol a -> a -> [[a]]
-- tal que (ramasCon a x) es la lista de las ramas del arbol a en las
-- que  aparece el elemento x. Por ejemplo,
-- ramasCon ej1 2 ==  [[5,2,1],[5,2,2],[5,3,2]]

-- SOLUCIONES:

-- data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
 
-- 1ª definicion
-- =============
 
ramasCon :: Eq a => Arbol a -> a -> [[a]]
ramasCon a x = [ys | ys <- ramas2 a, x `elem` ys]
-- 2ª definicion
-- =============
 
ramasCon2 :: Eq a => Arbol a -> a -> [[a]]
ramasCon2 a x = filter (x `elem`) (ramas2 a)
 
ramas2 :: Arbol a -> [[a]]
ramas2 (H x)     = [[x]]
ramas2 (N x i d) = map (x:) (ramas2 i ++ ramas2 d)

-- --------------------------------------------------------------------
-- Definir por  COMPRENSION Y RECURSION la funcion    3PUNTOS
-- siguiente :: Eq a => a -> [a] -> Maybe a
-- tal que (siguiente x ys) es justo el elemento siguiente a la primera
-- ocurrencia de x en ys o Nothing si x no pertenece a ys. 
-- Por ejemplo:
-- siguiente 5 [3,5,2,5,7]                       ==  Just 2
-- siguiente 9 [3,5,2,5,7]                       ==  Nothing
-- siguiente 'd' "afdegdb"                       ==  Just 'e'
-- siguiente "todo" ["En","todo","la","medida"]  ==  Just "la"
-- siguiente "nada" ["En","todo","la","medida"]  ==  Nothing
-- siguiente 999999 [1..1000000]                 ==  Just 1000000
-- siguiente 1000000 [1..1000000]                ==  Nothing

-- 1ª solucion (por recursion):
siguiente1 :: Eq a => a -> [a] -> Maybe a
siguiente1 x (y1:y2:ys) | x == y1   = Just y2
                        | otherwise = siguiente1 x (y2:ys)
siguiente1 x _ = Nothing
 
-- 2ª solucion (por comprension):
siguiente2 :: Eq a => a -> [a] -> Maybe a
siguiente2 x ys 
    | null zs   = Nothing
    | otherwise = Just (snd (head zs))
    where zs = [(u,v) | (u,v) <- zip ys (tail ys), u == x]  

-- --------------------------------------------------------------------
-- CON TAKE Y DROP. RECURSION. 3PUNTOS
-- Definir la funcion
-- actualizaUno :: [a] -> [(Int,a)] -> [a]
-- tal que (actualizaUno xs (n,y)) es la lista obtenida sustituyendo el 
-- elemento n-esimo de xs por y. Por ejemplo, 
--    actualizaUno [3,5,8,4] (2,1) ==  [3,5,1,4]
actualizaUno :: [a] -> (Int,a) -> [a]
actualizaUno xs (n,y) =
    take n xs ++ y : drop (n+1) xs

-- Definir la funcion 
-- actualiza :: [a] -> [(Int,a)] -> [a]
-- tal que (actualiza xs ps) es la lista obtenida sustituyendo en xs los
-- elementos cuyos indices son las primeras componentes de ps por las
-- segundas.  Por ejemplo,
-- actualiza [3,5,8,4] [(2,1),(0,7)]  ==  [7,5,1,4]

actualiza :: [a] -> [(Int,a)] -> [a]
actualiza xs []         = xs
actualiza xs ((n,y):ps) = actualiza (actualizaUno xs (n,y)) ps
 

