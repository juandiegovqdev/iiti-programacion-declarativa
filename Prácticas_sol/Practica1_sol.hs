--import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Evalúa las siguientes líneas para entender cómo funciona
-- el sistema de tipos que proporciona Haskell.
-- ---------------------------------------------------------------------
-- :type True       -- En Haskell tenemos el tipo Bool
-- :t True          -- :t y :type es lo mismo
-- :t 1             -- Haskell infiere el tipo más genérico posible, de la clase Num
-- :t 1.1
-- :t 'a'           -- Un carácter
-- :t "a"           -- Un String, o que es lo mismo [Char]
-- :t [1,2]         -- Una lista de elementos de un tipo cualquiera de la clase Num
-- :t [1,2.1]       -- Ahora los tipos deben ser Fractional, ya que hay decimales
-- :t [1,'a']       -- no es posible mezclar tipos en una lista
-- :t (1,'s')       -- con tuplas si es posible mezclar tipos
-- :t [[1],[1,2]]   -- Podemos tener lista de listas
-- :t not           -- Un operador sobre booleanes
-- :t sum           -- Considera que Foldable t es un tipo recorrible, por lo general, una lista
-- :t (+)           -- Un operador sobre Num
-- :t []            -- Una lista vacía de cualquier cosa
-- :t ()
-- :t (3+)          -- Un operador dado de forma parcial (ya hay un argumento, espera el otro)
-- :t length        -- Siempre devuelve length, y le da igual el tipo dentro de la lista
-- :t zip           -- Unir dos listas
-- :t take          -- A take le da igual el tipo que haya dentro de la lista

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sin evaluar las expresiones en GHC, decide qué tipo es  
-- el adecuado para cada una de ellas. Intenta dar el tipo más general.
-- ---------------------------------------------------------------------

-- i1:: Integer  -- El primero va de regalo
i1 = 45
-- i2 :: [Char]
i2 = "123"
-- i3 :: Bool
i3 = 45 <= i1
-- i4 :: Char
i4 = 'c'
-- i5 :: [[Char]]
i5 = ["abc","ok"]
-- i6 :: [Char]
i6 = head i5
-- i7 :: [Char]
i7 = tail "abc"
-- i8 :: Fractional a => (Bool,a)
i8 = (True,4.5)
-- i9 :: Num a => [a]
i9 = [i1,34]
-- i10 :: (Foldable t, Num a) => t a -> a
i10 = sum
-- (Num a, Enum a) => a -> Int 
i11 x = length [1..x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Para cada una de las siguientes expresiones, reemplaza
-- undefined por una expresión válida para el tipo que la declara.
-- ---------------------------------------------------------------------

j1:: (String,Integer)
j1 = ("Salu",2)

j2:: [Integer]
j2 = [1..5]

j3:: Char
j3 = 'd'

j4:: Double
j4 = 3.43

j5:: (Integer,String,Integer,Char)
j5 = (1,"a",2,'b')

j6:: ([Char],(Bool,String))
j6 = ("Hola",(True,"Adiós"))

j7:: [[Bool]]
j7 = [[True,False],[],[False,False,True]]

j8:: [(String,Bool)]
j8 = [("aa",True),("bb",False)]

j9:: Integer -> Integer
j9 = (+1)

j10:: Float -> [Bool] -> Bool
j10 f l = f > fromIntegral (length l) 

j11:: [Char] -> [[Int]]
j11 s = [[1..n] | n <- [1..length s]]


{-
Problema 1:

  Conocemos el cambio actual del euro a dólares estadounidenses: 1
  Euro son 1.17507 dólares

  * Definir la constante tipoCambio con dicho valor.
-}

tipoCambio :: Double
tipoCambio = 1.17507

{--
  * Calcular el cambio a dólares de distintas cantidades de euros y viceversa

  * Definir dos funciones, aEuros y aDolares, que dada una cantidad de
    dólares (resp. euros) permita obtener la cantidad de euros (resp.
    dólares) equivalente. 

    Nota: No es necesario redondear el resultado.

  * Volver a calcular los cambios anteriores utilizando las funciones
    definidas.
-}

aDolares :: Double -> Double
aDolares = (*tipoCambio)

aEuros :: Double -> Double
aEuros = (/tipoCambio)

{-

  * Escribir la siguiente propiedad: dada cualquier cantidad de euros,
    si la cambiamos a dólares y los dólares obtenidos los volvemos a
    cambiar a euros, obtenemos la cantidad de euros original.

  * Si la propiedad anterior ha fallado analiza el posible problema y
    busca Escribir la siguiente propiedad: dada cualquier cantidad de
    euros, una solución al mismo.
-}

propCambio :: Double -> Bool
propCambio euros = aEuros (aDolares euros) == euros

-- con 1.33 falla, por culpa del redondeo
propCambio' :: Double -> Bool
propCambio' euros = aEuros (aDolares euros) - euros < (10**(-5))

-- Otra versión definiendo un nuevo operador
(~==) :: (Num a, Ord a) => a -> a -> Bool
x ~== y = abs (x - y) < 1e-3

propCambioAceptable :: Double -> Bool
propCambioAceptable euros = aEuros (aDolares euros) ~== euros
prop_CambioAceptable euros = aEuros (aDolares euros) ~== euros

{-
  * Utilizar :browse para conocer los tipos de las definiciones
    anteriores y añadírselos a cada una.
--}

{--
Indicación:
Para cada una de las constantes y funciones que se definan a
continuación usar :t para averiguar el tipo que infiere haskell y
añadirlo a la definición (rectificándolo cuando sea conveniente).
--}

{--
Problema 2:

  Conocemos que 0ºC se corresponden con 32ªF y que un incremento de
  5ºC suponen un incremento de 9ºF.

  * Definir una función que permita pasar de ºC a ºF (y otra para el
    cambio contrario).

  * Si para mañana está prevista un mínimo de 19ºC y un máximo de
    34ºC, ¿cuál sería el rango expresado en ºF?
--}

-- c2f :: Fractional a => a -> a
c2f :: Double -> Double
c2f c = (c * (9/5)) + 32

-- f2c :: Fractional a => a -> a
f2c :: Double -> Double
f2c f = (f - 32) * (5/9)

-- (c2f 19, c2f 34) ==> (66.2 , 93.2)


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

precioBase :: Double
precioBase = 2.70

precioRed :: Double
precioRed = 2.20

calculaPrecioBase :: Int -> Double
calculaPrecioBase n = fromIntegral n * precioBase

calculaPrecioRed :: Int -> Double
calculaPrecioRed n =
  calculaPrecioBase x + fromIntegral y * precioRed
  where
    x = min 5 n
    y = max (n-5) 0

precioSuperRed :: Double
precioSuperRed = 2

calculaPrecioSup :: Int -> Double
calculaPrecioSup n =
  calculaPrecioRed x + fromIntegral y * precioSuperRed
  where
    x = min 10 n
    y = max (n-10) 0


{--
Problema 4:

  Consideremos el siguiente juego: Dado un número mayor que 1, si es
  par divídelo entre 2 y si es impar multiplícalo por 3 y súmale 1.
  Si el resultado es 1 ya has terminado, en caso contrario repite el
  procedimiento sobre el resultado.
-}

siguiente n
  | n == 1 = -1
  | even n = div n 2
  | otherwise = n*3+1
    
siguiente' n =
  case n of
    1 -> -1
    n
      | even n -> div n 2
      | otherwise -> n*3+1

{-
  Pregunta: Dado un número inicial cualquiera, cuántas veces tendrás
  que aplicar el procedimiento.
-}

veces 1 = 0
veces n = 1 + veces (siguiente n)

resultados n = tail (resultadosAux n)

resultadosAux 1 = [1]
resultadosAux n = n:resultadosAux (siguiente n)

{-
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
