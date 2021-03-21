-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 3 Parcial                                      21 de Diciembre 2017
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- # Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- # Asegúrese de utilizar exactamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite
--   describiendo claramente su objetivo.
-- -------------------------------------------------------------------

import Test.QuickCheck

-- -------------------------------------------------------------------
-- Ejercicio 3 (2 ptos).
-- -------------------------------------------------------------------
-- Una lista de números enteros se llama alternada si sus elementos
-- son alternativamente par/impar o impar/par.
-- -------------------------------------------------------------------
-- (3.1) Definir la función, utilizando el plegado foldl,

alternada :: [Int] -> Bool

-- tal que (alternada xs) reconoce si xs es una lista alternada.
-- Por ejemplo: 
--    alternada [1,2,3]     == True
--    alternada [1,2,3,4]   == True
--    alternada [8,1,2,3,4] == True
--    alternada [8,1,2,3]   == True
--    alternada [8]         == True
--    alternada [7]         == True
-- -------------------------------------------------------------------

-- Solución:

-- ---------------------------
-- Primera versión: Recursiva
-- ---------------------------
-- alternada (x:y:xs)
--   | even x    = odd y  && alternada (y:xs)
--   | otherwise = even y && alternada (y:xs)
-- alternada _ = True

-- ---------------------------
-- Segunda versión: Dividiendo la lista en dos listas, una con los
-- elementos de las posiciones pares y otra con el resto.
-- ---------------------------
-- alternada ls = (todos even l1 && todos odd l2) ||
--                (todos odd l1 && todos even l2)
--   where (l1, l2) = dividir [] [] ls

-- dividir :: [a] -> [a] -> [a] -> ([a], [a])
-- dividir ac1 ac2 (x1:x2:ls)  = dividir (x1:ac1) (x2:ac2) ls
-- dividir ac1 ac2 [] = (ac1, ac2)
-- dividir ac1 ac2 [x] = (x:ac1, ac2)

-- todos :: (a -> Bool) -> [a] -> Bool
-- todos p = foldl (\ac x -> p x && ac) True

-- ---------------------------
-- Tercera versión: Utilizando plegado (incluimos la versión por
-- recursión terminal) sobre una lista que empareja cada elemento con
-- su siguiente.
-- ---------------------------
-- alternada [] = True
-- -- alternada xs@(_:ls) = alternadaRT True (zip xs ls)
-- alternada xs@(_:ls) = alternadaL (zip xs ls)

-- -- alternadaRT :: Bool -> [(Int, Int)] -> Bool
-- -- alternadaRT ac [] = ac
-- -- alternadaRT ac ((x, y):ls) =
-- --   alternadaRT ((if even x then odd y else even y) && ac) ls

-- alternadaL :: [(Int, Int)] -> Bool
-- alternadaL =
--   foldl (\ac (x, y) ->
--           (if even x then odd y else even y) && ac)
--   True

-- ---------------------------
-- Cuarta versión: Utilizando plegado (incluimos la versión por
-- recursión terminal) sobre una lista que empareja cada elemento con
-- su posición (o con la posición del siguiente).
-- ---------------------------
-- alternada [] = True
-- -- alternada xs@(_:ls) =
-- --   alternadaRT True (zip xs [1..]) || alternadaRT True (zip xs [2..]) 
-- alternada xs@(_:ls) = alternadaL (zip xs [1..]) || alternadaL (zip xs [2..])

-- -- alternadaRT :: Bool -> [(Int, Int)] -> Bool
-- -- alternadaRT ac [] = ac
-- -- alternadaRT ac ((x, i):ls) =
-- --   alternadaRT ((if even i then even x else odd x) && ac) ls

-- alternadaL :: [(Int, Int)] -> Bool
-- alternadaL =
--   foldl (\ac (x, i) ->
--           (if even i then even x else odd x) && ac)
--   True

-- ---------------------------
-- Quinta versión: Utilizando plegado (incluimos la versión por
-- recursión terminal) sobre una lista que empareja cada elemento con
-- el predica que debe verificar.
-- ---------------------------

alternada [] = True
-- alternada xs@(_:ls) =
--   alternadaRT True (zip xs (cycle [even, odd])) ||
--   alternadaRT True (zip xs (cycle [odd, even])) 
alternada xs@(_:ls) =
  alternadaL (zip xs (cycle [even, odd])) ||
  alternadaL (zip xs (cycle [odd, even]))

-- alternadaRT :: Bool -> [(a, a -> Bool)] -> Bool
-- alternadaRT ac [] = ac
-- alternadaRT ac ((x, r):ls) = alternadaRT ((r x) && ac) ls

alternadaL :: [(a, a -> Bool)] -> Bool
alternadaL = foldl (\ac (x, r) -> (r x) && ac) True

-- -------------------------------------------------------------------
-- Generalizando, una lista es alternada respecto de un predicado p si
-- sus elementos verifican alternativamente el predicado p.
-- -------------------------------------------------------------------
-- (3.2) Definir la función, utilizando el plegado foldr,

alternadaG :: (a -> Bool) -> [a] -> Bool

-- tal que (alternadaG p xs) compruebe si xs es una lista alternada
-- respecto de p. Por ejemplo,
--    alternadaG (>0) [-2,1,3,-9,2]  == False
--    alternadaG (>0) [-2,1,-3,9,-2] == True
--    alternadaG (<0) [-2,1,-3,9,-2] == True
--    alternadaG even [8,1,2,3]      == True
-- -------------------------------------------------------------------

-- Solución:

-- ---------------------------
-- Primera versión: Recursiva
-- ---------------------------
-- alternadaG p (x:y:xs)
--   | p x    = not (p y)  && alternadaG p (y:xs)
--   | otherwise = p y && alternadaG p (y:xs)
-- alternadaG _ _ = True

-- ---------------------------
-- Segunda versión: Dividiendo la lista en dos listas, una con los
-- elementos de las posiciones pares y otra con el resto.
-- ---------------------------
-- alternadaG p ls = (todos p l1 && todos (not . p) l2) ||
--                 (todos (not . p) l1 && todos p l2)
--   where (l1, l2) = dividir [] [] ls

-- dividir :: [a] -> [a] -> [a] -> ([a], [a])
-- dividir ac1 ac2 (x1:x2:ls)  = dividir (x1:ac1) (x2:ac2) ls
-- dividir ac1 ac2 [] = (ac1, ac2)
-- dividir ac1 ac2 [x] = (x:ac1, ac2)

-- todos :: (a -> Bool) -> [a] -> Bool
-- todos r = foldr (\x fls -> r x && fls) True

-- ---------------------------
-- Tercera versión: Utilizando plegado (incluimos la versión por
-- recursión) sobre una lista que empareja cada elemento con su
-- siguiente.
-- ---------------------------
alternadaG _ [] = True
-- alternadaG p xs@(_:ls) = alternadaGR p (zip xs ls)
alternadaG p xs@(_:ls) = alternadaGL p (zip xs ls)

-- alternadaGR :: (a -> Bool) -> [(a, a)] -> Bool
-- alternadaGR p [] = True
-- alternadaGR p ac ((x, y):ls) =
--   (if p x then not (p y) else p y) && alternadaGR ls

alternadaGL :: (a -> Bool) -> [(a, a)] -> Bool
alternadaGL p =
  foldr (\(x, y) fls ->
          (if p x then not (p y) else p y) && fls)
  True

-- ---------------------------
-- Cuarta versión: Utilizando plegado (incluimos la versión por
-- recursión) sobre una lista que empareja cada elemento con su
-- posición (o con la posición del siguiente).
-- ---------------------------
-- alternadaG _ [] = True
-- -- alternadaG p xs@(_:ls) =
-- --   alternadaGR p (zip xs [1..]) ||
-- --   alternadaGR p (zip xs [2..]) 
-- alternadaG p xs@(_:ls) =
--   alternadaGL p (zip xs [1..]) || alternadaGL p (zip xs [2..])

-- -- alternadaGR :: [(Int, Int)] -> Bool
-- -- alternadaGR [] = True
-- -- alternadaGR ((x, i):ls) =
-- --   (if even i then p x else not (p x)) && alternadaGR ls

-- alternadaGL :: (a -> Bool) -> [(a, Int)] -> Bool
-- alternadaGL p =
--   foldr (\(x, i) fls ->
--           (if even i then p x else not (p x)) && fls)
--   True

-- ---------------------------
-- Quinta versión: Utilizando plegado (incluimos la versión por
-- recursión) sobre una lista que empareja cada elemento con el
-- predica que debe verificar.
-- ---------------------------
-- alternadaG _ [] = True
-- -- alternadaG p xs@(_:ls) =
-- --   alternadaGR (zip xs (cycle [p, not . p])) ||
-- --   alternadaGR (zip xs (cycle [not . p, p])) 
-- alternadaG p xs@(_:ls) =
--   alternadaGL (zip xs (cycle [p, not . p])) ||
--   alternadaGL (zip xs (cycle [not . p, p]))

-- -- alternadaGR :: [(a, a -> Bool)] -> Bool
-- -- alternadaGR [] = True
-- -- alternadaGR ((x, r):ls) = r x && alternadaGR ls

-- alternadaGL :: [(a, a -> Bool)] -> Bool
-- alternadaGL = foldr (\(x, r) fls -> r x && fls) True

-- -------------------------------------------------------------------
-- (3.3) Redefinir la función alternada usando alternadaG y comprobar
-- con QuickCheck que ambas definiciones coinciden.
-- -------------------------------------------------------------------

-- Solución:

alternada3 :: [Int] -> Bool
alternada3 = alternadaG even

-- La propiedad es
propAlternada :: [Int] -> Bool
propAlternada xs = alternada xs == alternada3 xs

-- Su comprobación es
--    ghci> quickCheck propAlternada
--    +++ OK, passed 100 tests.

-- -------------------------------------------------------------------
-- (3.4) Un número de la suerte es un número natural que se genera por
-- una criba como se indica a continuación:
-- 
-- Se comienza con la lista de los números enteros a partir de 1:
--    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25...
-- Se eliminan los números de dos en dos
--    1,  3,  5,  7,  9,   11,   13,   15,   17,   19,   21,   23,   25...
-- Como el segundo número que ha quedado es 3, se eliminan los números
-- restantes de tres en tres:  
--    1,  3,      7,  9,         13,   15,         19,   21,         25...
-- Como el tercer número que ha quedado es 7, se eliminan los números
-- restantes de siete en siete:   
--    1,  3,      7,  9,         13,   15,               21,         25...
-- 
-- Este procedimiento se repite indefinidamente y los supervivientes son
-- los números de la suerte:  
--    1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79
--
-- Definir la sucesión
--    numerosDeLaSuerte :: [Int]
-- cuyos elementos son los números de la suerte. Por ejemplo,
--    ghci> take 20 numerosDeLaSuerte
--    [1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79]
--    ghci> numerosDeLaSuerte !! 1500
--    13995
-- -------------------------------------------------------------------

-- Solución:

numerosDeLaSuerte :: [Int]
numerosDeLaSuerte = criba 3 [1,3..]
  where
    criba i (n:s:xs) =
      n : criba (i + 1) (s : [x | (n, x) <- zip [i..] xs , rem n s /= 0])
-- -------------------------------------------------------------------
