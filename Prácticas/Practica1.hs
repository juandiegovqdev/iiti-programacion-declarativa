-- PD-Practica 1
-- Definiciones de funciones, tipos y clases.
-- =====================================================================

-- A continuación se importa el módulo QuickCheck. Necesita ser instalado
-- previamente con Cabal o Stack
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Evalúa las siguientes líneas para entender cómo funciona
-- el sistema de tipos que proporciona Haskell.
-- ---------------------------------------------------------------------
-- :type True
-- :t True
-- :t 1
-- :t 1.1
-- :t 'a'
-- :t "a"
-- :t [1,2]
-- :t [1,2.1]
-- :t [1,'a']
-- :t (1,'s')
-- :t [[1],[1,2]] 
-- :t not
-- :t sum
-- :t (+)
-- :t []
-- :t ()
-- :t (3+)
-- :t length
-- :t zip
-- :t take

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sin evaluar las expresiones en GHC, decide qué tipo es  
-- el adecuado para cada una de ellas. Intenta dar el tipo más general.
-- ---------------------------------------------------------------------

i1:: Integer  -- El primero va de regalo
i1 = 45

i2 :: String
i2 = "123"

i3 :: Bool
i3 = 45 <= i1

i4 :: Char
i4 = 'c'

i5 :: [String]
i5 = ["abc","ok"]

i6 :: String
i6 = head i5

i7 :: [Char]
i7 = tail "abc"

i8 :: (Bool, Float)
i8 = (True,4.5)

i9 :: [Integer]
i9 = [i1,34]

i11 :: Int -> Int
i11 x = length [1..x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Para cada una de las siguientes expresiones, reemplaza
-- undefined por una expresión válida para el tipo que la declara.
-- ---------------------------------------------------------------------

j1:: (String,Integer)
j1 = ("Holaaaa", 1)

j2:: [Integer]
j2 = [1, 2, 3, 4, 5, 6, 7, 8]

j3:: Char
j3 = 'c'

j4:: Double
j4 = 0.2

j5:: (Integer,String,Integer,Char)
j5 = (1, "Hola", 2, 'd')

j6:: ([Char],(Bool,String))
j6 = (['a', 'b', 'c'], (True, "Hellooo"))

j7:: [[Bool]]
j7 = [[True, False], [True, False], [True, False]]

j8:: [(String,Bool)]
j8 = [("Holaa", True), ("Holaa", True), ("Holaa", True), ("Holaa", True)]

j9:: Integer -> Integer
j9 x = x

j10:: Float -> [Bool] -> Bool
j10 i (x:xs) = x

j11 :: [Char] -> [[Int]]
j11 _ = [[1, 2, 3, 4, 5]]

{--
Problema 1:

  Conocemos el cambio actual del euro a dólares estadounidenses: 1
  Euro son 1.17507 dólares

  * Definir la constante tipoCambio con dicho valor.

  * Calcular el cambio a dólares de distintas cantidades de euros y viceversa

  * Definir dos funciones, aEuros y aDolares, que dada una cantidad de
    dólares (resp. euros) permita obtener la cantidad de euros (resp.
    dólares) equivalente. 

    Nota: No es necesario redondear el resultado.

  * Volver a calcular los cambios anteriores utilizando las funciones
    definidas.

  * Escribir la siguiente propiedad: dada cualquier cantidad de euros,
    si la cambiamos a dólares y los dólares obtenidos los volvemos a
    cambiar a euros, obtenemos la cantidad de euros original.

  * Si la propiedad anterior ha fallado analiza el posible problema y
    busca Escribir la siguiente propiedad: dada cualquier cantidad de
    euros, una solución al mismo.

  * Utilizar :browse para conocer los tipos de las definiciones
    anteriores y añadírselos a cada una.
--}

tipoCambio :: Float 
tipoCambio = 1.17507 

aEuros :: Float -> Float
aEuros x = x/tipoCambio

aDolares :: Float -> Float
aDolares x = x * tipoCambio

propEuros :: Float -> Bool
propEuros x = x == aEuros (aDolares x) 

{--
Problema 2:

  Conocemos que 0ºC se corresponden con 32ªF y que un incremento de
  5ºC suponen un incremento de 9ºF.

  * Definir una función que permita pasar de ºC a ºF (y otra para el
    cambio contrario).

  * Si para mañana está prevista un mínimo de 19ºC y un máximo de
    34ºC, ¿cuál sería el rango expresado en ºF?
--}

relacionCentigradosAFarenheit :: Integer -> Integer
relacionCentigradosAFarenheit x = x * (div 9 5)

relacionFarenheitACentigrados :: Integer -> Integer
relacionFarenheitACentigrados x = x * (div 5 9)

centigradosAFarenheit :: Integer -> Integer
centigradosAFarenheit x = 32 + relacionCentigradosAFarenheit x

farenheitACentigrados :: Integer -> Integer
farenheitACentigrados x = relacionCentigradosAFarenheit (x-32)

{--
Problema 3:

  Una tienda vende las mallas de 2kg de patatas a 2.70 euros. Para 
  favorecer la venta de cantidades mayores ofrece un precio reducido
  de 2.20 euros a partir de la quinta malla. Es decir, si un cliente
  compra 18 mallas, las cinco primeras las cobra a 2.70 y las 13
  restantes a 2.20.

  * Definir una función que, dada la cantidad de mallas calcule el
    precio sin tener en cuenta la promoción. Calcular el precio del
    ejemplo proporcionado.

  * Definir una función que, dada la cantidad de mallas, calcular el
    precio correspondiente según la promoción. Usar dicha función
    para calcular, de nuevo, el precio del ejemplo.

  La oferta ha tenido tanto éxito que el vendedor decide ampliarla
  reduciendo el precio a 2 euros a partir de la décima malla.

  * Definir una función para la nueva promoción y volver al calcular
    el precio del ejemplo.
--}

calculoSinPromocion :: Float -> Float
calculoSinPromocion x = x * 2.7

calculoConPromocion :: Float -> Float
calculoConPromocion x 
  | x > 5 = 5 * 2.7 + (x-5) * 2.2
  | otherwise = x * 2.7

calculoSegundaPromocion :: Float -> Float
calculoSegundaPromocion x
  | x <= 5            = calculoSinPromocion x 
  | x > 5 && x <= 10  = calculoConPromocion x
  | otherwise         = 5 * 2.7 + (x - 5) * 2.2 + (x - 10) * 2
 
{--
Problema 4:

  Consideremos el siguiente juego: Dado un número mayor que 1, si es
  par divídelo entre 2 y si es impar multiplícalo por 3 y súmale 1.
  Si el resultado es 1 ya has terminado, en caso contrario repite el
  procedimiento sobre el resultado.

  Pregunta: Dado un número inicial cualquiera, cuántas veces tendrás
  que aplicar el procedimiento.

  Ejemplos:

  Si empezamos por 10 => dividimos por 2 y obtenemos 5 =>
  multiplicamos por 3 y sumamos 1, obteniendo 16 => toca volver a
  dividir y obtenemos 8 => repetimos y obtenemos 4 => seguimos y
  obtenemos 2 => alcanzamos el 1.

  los valores han sido 5, 16, 8, 4, 2, 1: lo hemos aplicado 6 veces

  Si empezamos por 7 los valores serán 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5,
  16, 8, 4, 2, 1: lo hemos aplicado 16 veces.


  * Definir una función que aplique una vez el procedimiento
    anterior. Utilizarla sucesivamente para verificar que los
    resultados proporcionados a partir de 10 y de 7 son correctos.

    Nota: Pueden ser de utilidad las funciones even y div

  * Definir una función que dado un número natural mayor que uno
    calcule el número de veces que se repite el resultado.

  * Definir una función que devuelva la lista de resultados hasta
    llegar  a 1.
--}

juegoNumeroImpar :: Integer -> Integer 
juegoNumeroImpar x = x * 3 + 1

juegoPar :: Integer -> Integer 
juegoPar x = div x 2

juego :: Integer -> Integer
juego x 
  | x == 1 = 0
  | odd x = 1 + juego (juegoNumeroImpar x)
  | otherwise = 1 + juego (juegoPar x)