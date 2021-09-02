-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 3 Parcial                                      21 de Diciembre 2017
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Una lista de números enteros se llama alternada si sus elementos
-- son alternativamente par/impar o impar/par.
-- -------------------------------------------------------------------
-- Definir la función, utilizando el plegado foldl,
--    alternada :: [Int] -> Bool
-- tal que (alternada xs) reconoce si xs es una lista alternada.
-- Por ejemplo: 
--    alternada [1,2,3]     == True
--    alternada [1,2,3,4]   == True
--    alternada [8,1,2,3,4] == True
--    alternada [8,1,2,3]   == True
--    alternada [8]         == True
--    alternada [7]         == True
-- -------------------------------------------------------------------

alternada :: [Int] -> Bool
alternada (x:y:xs)
    | even x    = odd y && alternada (y:xs)
    | otherwise = even y && alternada (y:xs)
alternada _ = True

-- -------------------------------------------------------------------
-- Generalizando, una lista es alternada respecto de un predicado p si
-- sus elementos verifican alternativamente el predicado p.
-- -------------------------------------------------------------------
-- (3.2) Definir la función, utilizando el plegado foldr,
--    alternadaG :: (a -> Bool) -> [a] -> Bool
-- tal que (alternadaG p xs) compruebe si xs es una lista alternada
-- respecto de p. Por ejemplo,
--    alternadaG (>0) [-2,1,3,-9,2]  == False
--    alternadaG (>0) [-2,1,-3,9,-2] == True
--    alternadaG (<0) [-2,1,-3,9,-2] == True
--    alternadaG even [8,1,2,3]      == True
-- -------------------------------------------------------------------

alternadaG :: (a -> Bool) -> [a] -> Bool
alternadaG p (x:y:xs)
    | p x       = p y /= True && alternadaG p (y:xs)
    | otherwise = p y == True && alternadaG p (y:xs)
alternadaG p _ = True

-- -------------------------------------------------------------------
-- (3.3) Redefinir la función alternada usando alternadaG y comprobar
-- con QuickCheck que ambas definiciones coinciden.
-- -------------------------------------------------------------------

prop_alternada xs = alternadaG even xs == alternada xs

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

numerosDeLaSuerte :: [Int]
numerosDeLaSuerte = eliminarNumerosSieteEnSiete (eliminarNumerosTresEnTres (eliminarNumerosDosEnDos [1..1000000]))

eliminarNumerosDosEnDos :: [Int] -> [Int]
eliminarNumerosDosEnDos (x:y:xs) = [x] ++ (eliminarNumerosDosEnDos xs) 
eliminarNumerosDosEnDos _ = []

eliminarNumerosTresEnTres :: [Int] -> [Int]
eliminarNumerosTresEnTres (x:y:z:xs) = [x] ++ [y] ++ (eliminarNumerosTresEnTres xs) 
eliminarNumerosTresEnTres _ = []

eliminarNumerosSieteEnSiete :: [Int] -> [Int]
eliminarNumerosSieteEnSiete (a:b:c:d:e:f:g:xs) = [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ (eliminarNumerosSieteEnSiete xs) 
eliminarNumerosSieteEnSiete _ = []

-- -------------------------------------------------------------------
-- Ejercicio 1.
-- Llamaremos lista asociativa a una lista de tuplas binarias. A los
-- primeros elementos de dichas tuplas los denominaremos claves, y a
-- los segundos valores. Por ejemplo:

listaEjemplo :: [(Int, String)]
listaEjemplo = [(1, "A"), (2, "B"), (2, "C")]

-- es una lista asociativa de enteros de precisión fija y cadenas de
-- caracteres. Las claves serían los números 1 y 2. Los valores las
-- cadenas "A", "B" y "C".

-- -------------------------------------------------------------------
-- (1.1) Definir, utilizando recursión, la función

esClave :: (Eq a) => a -> [(a, b)] -> Bool
esClave a ((x, y):xs)
    | a == x = True
    | otherwise = esClave a xs
esClave a [] = False

-- que, dado un elemento c y una lista asociativa l, determine si c
-- es una clave de l. Por ejemlo:
--   esClave 1 listaEjemplo ==> True
--   esClave 3 listaEjemplo ==> False
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (1.2) Definir la función

asoc1 :: (Eq a) => a -> [(a, b)] -> b
asoc1 a ((x, y):xs) 
    | x == a    = y
    | otherwise = asoc1 a xs
asoc1 _ [] = error "Clave no válida."

-- que, dados un elemento c y una lista asociativa l, devuelva un
-- elemento v tal que el par (c, v) pertenece a l. Análogamente,
-- definir la función

asoc2 :: (Eq b) => b -> [(a, b)] -> a
asoc2 a ((x, y):xs) 
    | y == a    = x
    | otherwise = asoc2 a xs
asoc2 _ [] = error "Valor no válido."

-- que , dados un elemento v y una lista asociativa l, devuelva un
-- elemento c tal que el par (c, v) pertenece a l. Por ejemplo:
--   asoc1 "butcher" [("butcher", "231 e22nd St."),
--                    ("baker", "515 w23rd St."),
--                    ("hardware", "988 Lexington Ave.")]
--   ==> "231 e22nd St."
--   asoc1 2 listaEjemplo ==> "B" (también sería
--         válida "C" como respuesta)
--   asoc1 [1] [([1,2,3], 3), ([3,3,3,4,5], 5), ([], 0)]
--   ==> *** Exception: asoc1: Clave no válida.
--   asoc2 3 [([1,2,3], 3), ([3,3,3,4,5], 5), ([3, 2, 1], 3)]
--   ==> [1,2,3] (también sería válida [3, 2, 1] como respuesta)
--   asoc2 "C" [(1, "A"), (2, "B"), (3, "C")] ==> 3
--   asoc2 "González Jiménez" [("Carlos", "Parrilla González"),
--                             ("Nuria", "García Jiménez"),
--                             ("Alicia", "García Granados")]  
--   ==> *** Exception: asoc2: Valor no válido.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (1.3) Definir

codigo :: [(Char, Int)]
codigo = [('a', 0),('b', 1),('c', 2),('d', 3),('e', 4),('f', 5),('g', 6),('h', 7),('i', 8),('j', 9),('k', 10),('l', 11),('m', 12),('n', 13),('ñ', 14),('o', 15),('p', 16),('q', 17),('r', 18),('s', 19),('t', 20),('u', 21),('v', 22),('w', 23),('x', 24),('y', 25),('z', 26)]

-- como una lista asociativa que represente la siguiente
-- correspondencia entre las 27 letras del alfabeto español (en
-- minúsculas) y los 27 primeros números naturales.
-- -------------------------------------------------------------------
-- Letra   a b c d e f g h i j  k  l  m  n  ñ  o  p  q  r  s  t  u  v
-- Número  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
-- -------------------------------------------------------------------
-- Letra   w  x  y  z
-- Número 23 24 25 26
-- -------------------------------------------------------------------

-- Se dice que una lista asociativa es un código si las claves no
-- aparecen repetidas y los valores tampoco.

-- -------------------------------------------------------------------
-- (1.4) Definir

esCodigo :: (Eq a, Eq b) => [(a, b)] -> Bool
esCodigo ((x,y):xs)
    | numeroRepeticionesClave ((x,y):xs) x == 1 && numeroRepeticionesValor ((x,y):xs) y == 1 = True && esCodigo xs
    | otherwise = False
esCodigo [] =  True

numeroRepeticionesClave :: (Eq a, Eq b) => [(a, b)] -> a -> Int
numeroRepeticionesClave ((x, y):xs) c
    | x == c    = 1 + numeroRepeticionesClave xs c
    | otherwise = numeroRepeticionesClave xs c
numeroRepeticionesClave [] _ = 0

numeroRepeticionesValor :: (Eq a, Eq b) => [(a, b)] -> b -> Int
numeroRepeticionesValor ((x, y):xs) c
    | c == y    = 1 + numeroRepeticionesValor xs c
    | otherwise = numeroRepeticionesValor xs c
numeroRepeticionesValor [] _ = 0

-- que, dada una lista asociativa, determine si es un código. Por
-- ejemplo:
--   esCodigo codigo ==> True
--   esCodigo listaEjemplo ==> False
--   esCodigo [(1, "C"), (2, "B"), (3, "C")] ==> False
-- -------------------------------------------------------------------
-- (1.5) Definir

letraNumero :: Char -> Int
letraNumero a = letraNumeroAux codigo a

letraNumeroAux :: [(Char, Int)] -> Char -> Int
letraNumeroAux ((x,y):xs) a
    | x == a = y
    | otherwise = letraNumeroAux xs a
letraNumeroAux [] _ = error "sólo disponible para el alfabeto español (minúsculas)."

-- que reciba una letra y devuelva la correspondencia numérica de dicha
-- letra según el código definido. De forma análoga, definir
                                                    
numeroLetra :: Int -> Char
numeroLetra a = numeroLetraAux codigo a

numeroLetraAux :: [(Char, Int)] -> Int -> Char
numeroLetraAux ((x,y):xs) a
    | y == a = x
    | otherwise = numeroLetraAux xs a
numeroLetraAux [] _ = error "sólo hay 27 letras disponibles (numeradas de 0 a 26)."

-- que reciba un número natural menor que 27 y devuelva la letra que
-- corresponde a dicho número. Por ejemplo:
--   letraNumero 'g' ==> 6
--   letraNumero 'n' ==> 3
--   letraNumero 'A' ==>
--   *** Exception: letraNumero: sólo disponible para el alfabeto
--   español (minúsculas).
--   numeroLetra 6   ==> 'g'
--   numeroLetra 13  ==> 'n'
--   numeroLetra 30  ==>
--   *** Exception: numeroLetra: sólo hay 27 letras disponibles
--   (numeradas de 0 a 26).
-- NOTA IMPORTANTE: Ambas definiciones deben estar basadas en la
-- correspondencia entre letras y números definida. Es decir, si se
-- cambia la definición de codigo por otra correspondencia del mismo
-- tipo, las funciones deben funcionar de acuerdo a la nueva
-- definición.
-- -------------------------------------------------------------------

-- Representaremos un mensaje como una lista cuyos elementos son los
-- caracteres alfabéticos que lo componen; es decir, descartamos los
-- espacios, los signos de puntuación y demás.
-- Por ejemplo, los siguientes mensajes:
--    Probando, probando.
--    Este mensaje se autodestruirá en cinco segundos.
--    La contraseña es: esos tipos con bigote, que parecen hotentotes.
-- vendrán representados por las siguientes listas:

type Mensaje = [Char]
mensaje1 :: Mensaje
mensaje1 = "probandoprobando"
mensaje2 :: Mensaje           
mensaje2 = "estemensajeseautodestruiraencincosegundos"
mensaje3 :: Mensaje
mensaje3 = "laclaveesesostiposconbigotesqueparecenotentotes"

-- Para codificar los mensajes utilizaremos la correspondencia entre
-- las letras del alfabeto y los primeros 27 números naturales dada
-- en el ejercicio anterior a través de las funciones letraNumero y
-- numeroLetra.

-- -------------------------------------------------------------------
-- Ejercicio 2. Cifrado por desplazamiento.
-- Dado un número natural k, cada letra del mensaje se reemplaza por
-- la letra que se encuentra k lugares a la derecha en el código. Por
-- ejemplo, para k = 3 la letra 'a' se reemplazará por 'd', la letra
-- 'b' por 'e', y así hasta la letra 'z' que se reemplaza por 'c'.
-- Nota: puede ser de utilidad la función mod.
-- -------------------------------------------------------------------
-- (2.1) Definir

desplaza :: Int -> Char -> Char
desplaza = undefined

-- que dados k y una letra devuelva la letra que se encuentra k
-- posiciones a la derecha en el código. Por ejemplo
--   desplaza 3 'a' ==> 'd'
--   desplaza 3 'z' ==> 'c'

-- -------------------------------------------------------------------
-- (2.2) Definir, sin utilizar recursión, 

cifradoDesplazamiento :: Int -> Mensaje -> Mensaje
cifradoDesplazamiento = undefined

-- que dados k y un mensaje devuelva el mensaje cifrado. Por ejemplo:
--   cifradoDesplazamiento 3 mensaje1 ==> "suredpgrsuredpgr"
--   cifradoDesplazamiento 0 mensaje3
--   ==> "laclaveesesostiposconbigotesqueparecenotentotes"
--   cifradoDesplazamiento 5 mensaje2
--   ==> "jxyjqjrxfñjxjfzytijxywznwfjrhnrhtxjlzritx"
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 3. Cifrado Vigenère
-- Utiliza una palabra clave p. Si dicha palabra tiene m letras, se
-- divide el mensaje en bloques de m letras. Y a cada bloque se le
-- suma la clave.
-- Por ejemplo, supongamos que usamos prueba como palabra clave y
-- queremos cifrar el mensaje 'Probando, probando'.
--    proban doprob ando
--  + prueba prueba prue
--  --------------------
--    fjjfbn sgkvpb pexs
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (3.1) Definir

sumaLetras :: Char -> Char -> Char
sumaLetras = undefined

-- que dadas dos letras calcule el resultado de 'sumarlas'. Por
-- ejemplo:
--   sumaLetras 'b' 'e' ==> 'f' ('b' es 1, 'e' es 4 y 'f' es 5).
--   sumaLetras 'd' 'p' ==> 's' ('d' es 3, 'p' es 16 y 's' es 19).
-- -------------------------------------------------------------------
-- (3.2) Definir

vigenere :: [Char] -> Mensaje -> Mensaje
vigenere = undefined

-- que dada una palabra clave (la lista de caracteres que la
-- componen) y un mensaje, devuelva el mensaje cifrado. Por ejemplo:
--   vigenere "mision" mensaje2
--   ==> "pammaqyasqsfpinbdppamzjudiwuquykhassguvwh"
--   vigenere "tebeo" mensaje3
--   ==> "eedoooifwsmstxwjstgdgfjkdnitujxtbvsviñsixqusixw"
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 4
-- El cifrado de un mensaje no cambia la longitud del mismo.
-- Comprobar utilizando quickCheck, definiendo las propiedades
-- adecuadas, que las funciones cifradoDesplazamiento y vigenere lo
-- verifican.



-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 5
-- (5.1) Definir, utilizando recursión

menoresR :: Int -> [[a]] -> [a]
menoresR a xss = concatMap (++[]) [xs | xs <- xss, length xs < a]

-- que, dado entero positivo k y una lista de listas lls, devuelva los
-- elementos de las listas ls de lls que tienen menos de k elementos.
-- Por ejemplo:
--   menoresR 3 [[], [1], [4, 4, 4, 4], [2, 2], [5, 5, 5, 5, 5]]
--   ==> [1, 2, 2]
--   menoresR 6 ["casa", "caminante", "ostracismo", "merienda", "socio"]
--   ==> "casasocio"

-- -------------------------------------------------------------------
-- Ejercicio 6
-- (6.1) Definir, proporcionando un tipo adecuado,

longitud :: (Double, Double) -> Double
longitud (a, b)
    | b < 0          = longitud (a, -b)
    | a < 0          = longitud (-a, b) 
    | otherwise      = a + b

-- que calcule la longitud del segmento de la recta real determinado
-- por los dos elementos del par. Por ejemplo:
--   longitud (3.5, (-2.7)) ==> 6.2
--   longitud ((-2.7), 3.5) ==> 6.2
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (6.2) Definir, proporcionando un tipo adecuado y con la
-- menor cantidad de ecuaciones posibles (sin utilizar guardas), 

cantidadVerdaderos :: (Bool, Bool, Bool) -> Int
cantidadVerdaderos (a, b, c) = devuelveVerdadero a + devuelveVerdadero b + devuelveVerdadero c

devuelveVerdadero :: Bool -> Int
devuelveVerdadero True  = 1
devuelveVerdadero False = 0

-- que, dada una tupla con tres valores lógicos, calcule cuántos son
-- verdaderos. Por ejemplo:
--  cantidadVerdaderos (5 < 0, 'a' == 'A', not False) ==> 1
--  cantidadVerdaderos (2 == 1 + 1,2 * 2 == 2 + 2,3 `elem` [1, 2, 3])
--  ==> 3
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 2.
-- -------------------------------------------------------------------
-- (2.1) Definir una función, utilizando recursión,

alterna :: [a] -> [a] -> [a]
alterna (x:xs) (y:ys) = x : y : alterna xs ys
alterna xs [] = xs
alterna [] ys = ys

-- tal que (alterna xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--   alterna [5,1] [2,7,4,9]     == [5,2,1,7,4,9]
--   alterna [5,1,7] [2..10]     == [5,2,1,3,7,4,5,6,7,8,9,10]
--   alterna [2..10] [5,1,7]     == [2,5,3,1,4,7,5,6,7,8,9,10]
--   alterna [2,4..12] [1,5..28] == [2,1,4,5,6,9,8,13,10,17,12,21,25]

-- -------------------------------------------------------------------
-- (2.2) Comprobar con QuickCheck que el número de elementos de
-- (alterna xs ys) es la suma de los números de elementos de xs e ys.

prop_alterna xs ys = length (alterna xs ys) == (length xs + length ys)

-- -------------------------------------------------------------------
-- Ejercicio 3. Definir la función

-- tal que (resultadoPositivo f xs) es la lista de los elementos de la
-- lista xs tales que el valor de la función f sobre ellos es
-- positivo. Por ejemplo,
-- Definir esta función

-- resultadoPositivoR head [[-1,2],[-9,4],[2,3]]       == [[2,3]]
-- resultadoPositivoR sum [[1,2],[9],[-8,3],[],[3,-5]] == [[1,2],[9]]
-- resultadoPositivoR (+ 3.5) [1,2,-9,-8.3,-3,-1]      == [1,2,-3,-1]
-- 1) por recursión (utilizando guardas)
resultadoPositivoR :: (Num b, Ord b) => (a -> b) -> [a] -> [a]
resultadoPositivoR f (x:xs)
    | f x > 0   = x : resultadoPositivoR f xs
    | otherwise = resultadoPositivoR f xs
resultadoPositivoR _ [] = []

-- resultadoPositivoP head [[-1,2],[-9,4],[2,3]]       == [[2,3]]
-- resultadoPositivoP sum [[1,2],[9],[-8,3],[],[3,-5]] == [[1,2],[9]]
-- resultadoPositivoP (+ 3.5) [1,2,-9,-8.3,-3,-1]      == [1,2,-3,-1]
-- 2) por plegado (con 'foldr')
resultadoPositivoP :: (Num b, Ord b) => (a -> b) -> [a] -> [a]
resultadoPositivoP = undefined

-- resultadoPositivoRec head [[-1,2],[-9,4],[2,3]]       == [[2,3]]
-- resultadoPositivoRec sum [[1,2],[9],[-8,3],[],[3,-5]] == [[1,2],[9]]
-- resultadoPositivoRec (+ 3.5) [1,2,-9,-8.3,-3,-1]      == [1,2,-3,-1]
-- 3) por recursión terminal
resultadoPositivoRec :: (Num b, Ord b) => (a -> b) -> [a] -> [a]
resultadoPositivoRec = undefined

-- -------------------------------------------------------------------
-- Ejercicio 4. Definir la función

-- tal que (alternos f g xs) es la lista obtenida aplicando
-- alternativamente las funciones f y g a los elementos de la lista
-- xs. Por ejemplo, 
-- alternos (+1) (*3) [1,2,3,4,5]                   == [2,6,4,12,6]
-- alternos (take 2) reverse ["todo","para","nada"]
--   == ["to","arap","na"]

alternos :: (a -> b) -> (a -> b) -> [a] -> [b]
alternos f g (x:y:xs) = f x : g y : alternos f g xs
alternos f g (x:xs) = f x : alternos f g xs
alternos f g _ = []

-- -------------------------------------------------------------------
-- Ejercicio 5. Definir la función

-- tal que (buscaCrucigrama x i l ps) es la lista de los elementos de
-- la lista de palabras ps, que tienen longitud l y tienen la letra x
-- en la posición 'i' (comenzando en 0). Por ejemplo,
-- buscaCrucigrama 'c' 1 7 ["ocaso", "casa", "ocupado"] == ["ocupado"]
-- Definir esta función utilizando filter y una función anónima.

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama x i l (p:ps)
    | length p == l && x == p!!i = p : buscaCrucigrama x i l ps
    | otherwise = buscaCrucigrama x i l ps
buscaCrucigrama _ _ _ [] = []

-- -------------------------------------------------------------------
-- Ejercicio 6. Definir la función

-- tal que (tresDiferentes x y z) se verifica si los elementos x, y y
-- z son distintos. Por ejemplo,
--    tresDiferentes 3 5 2  ==  True
--    tresDiferentes 3 5 3  ==  False

tresDiferentes :: (Eq a) => a -> a -> a -> Bool
tresDiferentes x y z 
    | x /= y && x /= z && y /= z = True
    | otherwise                  = False

-- -------------------------------------------------------------------
-- Ejercicio 7. Los resultados de las votaciones a delegado en un
-- grupo de clase se recogen mediante listas de asociación.
-- -------------------------------------------------------------------
-- (7.1) Definir el tipo Resultados de tal forma que la siguiente
-- definición sea correcta.

type Resultados = [(String, Int)]

votos :: Resultados
votos =  [("Ana Perez",10),("Juan Lopez",7), ("Julia Rus", 27), ("Pedro Val",1), ("Pedro Ruiz",27),("Berta Gomez",11)]
 
-- -------------------------------------------------------------------
-- (7.2) Define la función

-- tal que (mayorV xs) devuelve el número de votos obtenido por los
-- ganadores de la votación cuyos resultados se recogen en xs. Por
-- ejemplo,
--     mayorV votos == 27

mayorV :: Resultados -> Int
mayorV xs = mayorVAux xs 0

mayorVAux :: Resultados -> Int -> Int
mayorVAux ((x,y):xs) a
    | y > a = mayorVAux xs y
    | otherwise = mayorVAux xs a
mayorVAux [] a = a

-- b) Define la función 

ganadores :: Resultados -> [String]
ganadores [] = []
ganadores ((x,y):xs) = if y == m then x : ganadores xs else ganadores xs
    where m = mayorV votos

-- tal que (ganadores xs) es la lista de los estudiantes con mayor
-- número de votos en xs. Por ejemplo,
--     ganadores votos == ["Julia Rus","Pedro Ruiz"]

-- -------------------------------------------------------------------
-- Ejercicio 8.
-- -------------------------------------------------------------------
-- (8.1) Definir

falsos :: [Bool]
falsos = [False | x <- [1..20000000]]

-- una lista infinita en la que todos los elementos son False.
-- take 6 falsos == [False,False,False,False,False,False]
-- take 11 falsos ==
-- [False,False,False,False,False,False,False,False,False,False,False]

-- -------------------------------------------------------------------
-- (8.2) Definir la función

indicesVerdaderos :: [Int] -> [Bool]
indicesVerdaderos = undefined

-- tal que (indicesVerdaderos xs) es la lista infinita de booleanos
-- tal que sólo son verdaderos los elementos cuyos índices pertenecen
-- a la lista estrictamente creciente xs. Por ejemplo,
-- take 6 (indicesVerdaderos [1,4])
-- == [False,True,False,False,True,False]
-- take 6 (indicesVerdaderos [0,2..])
-- == [True,False,True,False,True,False]
-- take 3 (indicesVerdaderos [])      == [False,False,False]
-- take 6 (indicesVerdaderos [1..])
-- == [False,True,True,True,True,True]
-- -------------------------------------------------------------------
