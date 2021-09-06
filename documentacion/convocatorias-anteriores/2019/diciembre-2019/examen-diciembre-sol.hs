-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Examen Diciembre                                   17 de Diciembre de 2019
-- --------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- --------------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · 1. Antes de continuar, cambie el nombre de este archivo por:
--                   diciembre_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · 2. Por favor, entregue un fichero que cargue correctamente, dejando
--   comentado todo código con errores.
-- · 3. Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · 4. Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- --------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Default
import Data.Aeson
import GHC.Generics
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

import Data.Matrix
import TADPila

-- --------------------------------------------------------------------------
-- Ejercicio 1. [2 ptos]
-- --------------------------------------------------------------------------
-- La función extremosCumplen recibe un predicado p, una función f y
-- una lista de listas xss, y devuelve una lista de pares de elementos
-- tal que, para cada lista de la lista de listas original, devuelva el par de
-- elementos resultantes de aplicar la función indicada sobre el menor y
-- mayor elemento que cumpla el predicado. Veámoslo a través de ejemplos:

--  λ> extremosCumplen even (^2) [[1..5],[(-9)..(-1)]]
--     [(4,16),(64,4)]
--
--  λ> extremosCumplen (\x -> length x > 3)
--                     (take 3)
--                     [["paco","es","poco","pico"],
--                      ["tres", "tristes", "tigres"]]
--     [("pac","poc"),("tig","tri")]

-- En definitiva, se pide definir la función anterior usando
-- distintas técnicas como núcleo de su solución:
--  a) Listas por comprensión: extremosCumplenL
--  b) Recursión no final: extremosCumplenR
--  c) Recursión con acumulador: extremosCumplenR2
--  d) Funciones de orden superior distintas de plegado: extremosCumplenO
--  e) Plegado: extremosCumplenP

extremosCumplen :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplen p f [] = []
extremosCumplen p f (xs:xss) = (f a, f b):extremosCumplen p f xss
  where a = minimum cumplen
        b = maximum cumplen
        cumplen = filter p xs
        
extremosCumplenR :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplenR p f [] = []
extremosCumplenR p f (xs:xss) = (f a, f b) : extremosCumplenR p f xss
  where a = minimum cumplen
        b = maximum cumplen
        cumplen = satisfacen p xs
        satisfacen p [] = []
        satisfacen p (x:xs) | p x = x : satisfacen p xs
                            | otherwise = satisfacen p xs

extremosCumplenL :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplenL p f xss = [extremos xs | xs <- xss]
  where extremos xs = (f a, f b)
          where a = minimum cumplen
                b = maximum cumplen
                cumplen = [x | x <- xs, p x]

extremosCumplenO :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplenO p f xss = map extremos xss
  where extremos xs = (f a, f b)
          where a = minimum cumplen
                b = maximum cumplen
                cumplen = filter p xs
        
extremosCumplenR2 :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplenR2 p f xss = aux [] xss
  where aux acc [] = acc
        aux acc (xs:xss) = aux (acc++[exs]) xss
          where exs = (f a, f b)
                  where a = minimum cumplen
                        b = maximum cumplen
                        cumplen = filter p xs

extremosCumplenP :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplenP p f xss = foldr (\xs y -> e xs : y) [] xss
  where e xs = (f a, f b)
          where a = minimum cumplen
                b = maximum cumplen
                cumplen = filter p xs
                              
extremosCumplenP2 :: (Ord a) => (a -> Bool) -> (a -> b) -> [[a]] -> [(b,b)] 
extremosCumplenP2 p f xss = foldl (\acc xs -> acc ++ [e xs]) [] xss
  where e xs = (f a, f b)
          where a = minimum cumplen
                b = maximum cumplen
                cumplen = filter p xs
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 2. [2 ptos]
-- --------------------------------------------------------------------------
-- 1. Defina, con sintaxis de registro, los tipos necesarios para almacenar
--    la información sobre personajes de Star Wars contenida en el
--    archivo personajes.json, ignorando aquellos datos que no son requeridos
--    para los apartados siguientes.
--
-- Nota: tenga en cuenta que los personajes se encuentran en un array dentro
--       de un campo de un objeto superior.
--
-- 2. Atribuya valores por defecto a los campos considerados.
--
-- 3. Realice un programa principal que:
--    a) Importe el archivo "personajes.json",
--    b) Indique el número total de personajes devuelto por la API
--    c) Para cada personaje, imprima por pantalla su nombre, altura, peso,
--       y número de películas en las que aparece
--    d) Indique el nombre y la estatura del personaje más alto devuelto.
-- -------------------------------------------------------------------

data Personaje = Pers {
  name::String, height::String, mass::String, films::[String]
  } deriving (Show, Generic)
    
instance Default Personaje where
  def = Pers {
    name="nobody", height="100", mass="100",films=def
    }
 
instance FromJSON Personaje

type Personajes = [Personaje]

data Contenedor = Cont {
  count::Integer, results::Personajes
  } deriving (Show, Generic)

instance Default Contenedor where
  def = Cont {
    count=def, results=def
    }

instance FromJSON Contenedor

main3 :: IO ()
main3 = do
  args <- getArgs
  let filename = if null args then "personajes2.json" else args!!0
  contents <- B.readFile filename

  let cont = decode contents :: Maybe Contenedor
  case cont of
    Just cont -> do
      putStrLn $ "Hay un total de " ++ show (count cont) ++ " personajes."
      let perss = results cont
      processPersonajes perss
      masAlto perss
    _ -> putStrLn "Not valid people"

processPersonajes :: Personajes -> IO ()
processPersonajes personajes = do
  mapM_ processPersonaje personajes

processPersonaje :: Personaje -> IO ()
processPersonaje personaje = do
  putStrLn $
    show (name personaje) ++
    " mide " ++ show (fromIntegral (read (height personaje)::Int) / 100) ++
    " metros, pesa " ++ show (read (mass personaje)::Int) ++
    " kilos y aparece en " ++ show (length (films personaje)) ++
    " pelis."

masAlto :: Personajes -> IO ()
masAlto personajes = do
  putStrLn $
    "El personaje de mayor altura es " ++ show a ++ ", que mide " ++
    show (fromIntegral b/100) ++ " metros."
  where (a,b) = (per h, h)
        h = maximum (map (\p -> read (height p)::Int) personajes)
        per he = name $ head (filter (\p -> height p == show he) personajes)
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]
-- --------------------------------------------------------------------------
-- Un árbol binario se puede codificar asociando valores tan solo en las
-- hojas. Se pide,
--  a) Definir el tipo de dato algebraico del árbol binario polimórfico con
-- valores solo en las hojas. Este tipo de ser imprimible e igualable.

data Arbol a = H a | N (Arbol a) (Arbol a)
  deriving (Show,Eq)

--  b) Definir la función (elemNivel a x), tal que determine el nivel menos
--  profundo donde suceda x. La raíz cuenta como nivel 0, sus hijos el nivel 1,
--  etc. Esta función debe devolver un Maybe, ya que si el elemento no se
--  encuentra, devuelve Nothing. Si lo encuentra, devuelve simplemene el nivel.
--  Por ejemplo,
--    λ> ejar1
--       N (H 1) (N (N (H 1) (H 2)) (H 1))
--    λ> elemNivel ejar1 2
--       Just 3
--    λ> elemNivel ejar1 1
--       Just 1
--    λ> elemNivel ejar1 5
--       Nothing

ejar1 :: Arbol Int
ejar1 = (N (H 1) (N (N (H 1) (H 2)) (H 1)))

elemNivel a x = elemNivel' a x 0

elemNivel' (H y) x n | x == y = Just n
                     | otherwise = Nothing
elemNivel' (N a1 a2) x n
  | en1 == Nothing = en2
  | en2 == Nothing = en1
  | otherwise = Just (min en1' en2')
  where en1 = elemNivel' a1 x (n+1)
        en2 = elemNivel' a2 x (n+1)
        Just en1' = en1
        Just en2' = en2
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 4. [2 ptos]
-- --------------------------------------------------------------------------
-- En el problema de las torres de Hanoi se consideran tres varillas donde se
-- apilan discos de distinto tamaño. Una forma de codificar las varillas es
-- el emplear el tipo de dato abstracto pila con enteros, donde cada entero
-- indica el tamaño del disco (de menor a mayor)

ejp1, ejp2, ejp3 :: Pila Int
ejp1 = foldr apila vacia [4,5,7,10]
ejp2 = foldr apila vacia [1,2,3,6,8,9]
ejp3 = foldr apila vacia [4,5,1,2]

-- Se pide:
--  a) Definir la función (comprueba p), donde p es una pila que codifica una
--  varilla de discos, y compruebe si la varilla es correcta; es decir, que
--  ningún disco tenga por encima otro de mayor tamaño. Por ejemplo,
--   λ> comprueba ejp2
--      True
--   λ> comprueba ejp3
--      False

comprueba :: Pila Int -> Bool
comprueba p | esVacia p = True
            | todosMayores cp dp = comprueba dp
            | otherwise = False
  where cp = cima p
        dp = desapila p
        todosMayores x q | esVacia q = True
                         | otherwise = (x < cima q) && todosMayores x (desapila q)

--  b) Definir la función (transfiere n p1 p2), donde p1 y p2 son dos varillas
--  con discos, n es un número natural, y devuelva el resultado de apilar en p2
--  los primeros n discos de p1, manteniendo el orden; es decir, asume que se
--  puede coger más de un dico a la vez y moverlos a la segunda varilla. Por
--  ejemplo, 
-- λ> transfiere 3 ejp2 ejp1
-- 1|2|3|4|5|7|10|-
-- λ> transfiere 9 ejp2 ejp1
-- 1|2|3|6|8|9|4|5|7|10|-

transfiere :: Int -> Pila Int -> Pila Int -> Pila Int
transfiere n p1 p2 | n==0 || esVacia p1 = p2
                   | otherwise = apila cp1 (transfiere (n-1) dp1 p2)
  where cp1 = cima p1
        dp1 = desapila p1
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 5. [2 ptos]
-- --------------------------------------------------------------------------
-- Representamos el tablero del ajedrez con una matriz donde los elementos son
-- del tipo Pieza. Este tipo tiene como posibles valores: V, C, T, A, P, RY y
-- RA, que significan vacío, caballo, torre, alfil, peón, rey y reina,
-- respectivamente. Se pide:
--   a) Definir el nuevo tipo de datos Pieza para que los ejemplos ejm1 y ejm2
-- se puedan cargar correctamente una vez descomentados. Aplica las
-- derivaciones necesarias para poder utilizarlo en el resto de ejercicios.

data Pieza = V | C | T | A | P | RY | RA
  deriving (Show,Eq)

ejm1 :: Matrix Pieza
ejm1 = fromLists [replicate 4 (V),
                 [C, V, C, V],
                 [V, V, A, V],
                 [V,RY,RA, T]]

ejm2 :: Matrix Pieza
ejm2 = fromLists [replicate 4 (V),
                 [C, C, C,RY],
                 [V, V, P,C],
                 [V, V,RA,T]]

--   b) Definir la función (jaquecaballo m), que compruebe si en el tablero
--   codificado en la matriz m existe algún caballo dando jaque al rey. Por
--   ejemplo,
--    λ> jaquecaballo ejm1
--       True
--    λ> jaquecaballo ejm2
--       False

jaquecaballo :: Matrix Pieza -> Bool
jaquecaballo m = or [ compruebaRey i j | i <- [1..nf], j <- [1..nc], m!(i,j) == C]
  where compruebaRey i j = (esRey (i-2) (j-1)) || (esRey (i-2) (j+1)) ||
                           (esRey (i+2) (j-1)) || (esRey (i+2) (j+1)) ||
                           (esRey (i-1) (j-2)) || (esRey (i+1) (j-2)) ||
                           (esRey (i-1) (j+2)) || (esRey (i+1) (j+2)) 
        esRey i j | i < 1 || i > nf || j < 1 || j > nc = False
                  | otherwise = m!(i,j) == RY
        nf = nrows m
        nc = ncols m
-- --------------------------------------------------------------------------
