-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 1 Parcial                                       8 de Noviembre 2017
-- -------------------------------------------------------------------

import Test.QuickCheck

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
letraNumero = undefined    

-- que reciba una letra y devuelva la correspondencia numérica de dicha
-- letra según el código definido. De forma análoga, definir
                                                    
numeroLetra :: Int -> Char
numeroLetra = undefined

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
menoresR = undefined

-- que, dado entero positivo k y una lista de listas lls, devuelva los
-- elementos de las listas ls de lls que tienen menos de k elementos.
-- Por ejemplo:
--   menoresR 3 [[], [1], [4, 4, 4, 4], [2, 2], [5, 5, 5, 5, 5]]
--   ==> [1, 2, 2]
--   menoresR 6 ["casa", "caminante", "ostracismo", "merienda", "socio"]
--   ==> "casasocio"

-- -------------------------------------------------------------------
-- (5.2) Definir, sin utilizar recursión

menoresA :: Int -> [[a]] -> [a]
menoresA = undefined

-- análoga a la anterior.
-- Por ejemplo:
--   menoresA 3 [[], [1], [4, 4, 4, 4], [2, 2], [5, 5, 5, 5, 5]]
--   ==> [1, 2, 2]
--   menoresA 6 ["casa", "caminante", "ostracismo", "merienda", "socio"]
--   ==> "casasocio"

-- -------------------------------------------------------------------
-- Ejercicio 6
-- (6.1) Definir, proporcionando un tipo adecuado,

longitud = undefined

-- que calcule la longitud del segmento de la recta real determinado
-- por los dos elementos del par. Por ejemplo:
--   longitud (3.5, (-2.7)) ==> 6.2
--   longitud ((-2.7), 3.5) ==> 6.2
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (6.2) Definir, proporcionando un tipo adecuado y con la
-- menor cantidad de ecuaciones posibles (sin utilizar guardas), 

cantidadVerdaderos = undefined

-- que, dada una tupla con tres valores lógicos, calcule cuántos son
-- verdaderos. Por ejemplo:
--  cantidadVerdaderos (5 < 0, 'a' == 'A', not False) ==> 1
--  cantidadVerdaderos (2 == 1 + 1,2 * 2 == 2 + 2,3 `elem` [1, 2, 3])
--  ==> 3
-- -------------------------------------------------------------------
