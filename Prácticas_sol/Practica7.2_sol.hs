-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Esta práctica trata de asentar algunos de los primeros conceptos
-- introducidos en el tema de entrada y salida con ficheros.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import System.Environment (getArgs)
import System.Directory
import Text.CSV
import Text.Printf

main0 :: IO ()
main0 = do
  putStrLn "Vamos con los ejercicios propuestos..."
  putStrLn "--------------------------------------"
  putStrLn "Comente la linea main=main0 y descomente las de ejercicios"
  putStrLn "posteriores para que su main vaya llamando a cada mainN"
  putStrLn "conforme vaya avanzando."

--main = main0

-- ---------------------------------------------------------------------
-- Ejercicio 1. Crear un programa que lea el fichero "lorem_ipsum.txt"
-- (descárguelo de http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt) 
-- y devuelva una tupla conteniendo:
--    * El número de párrafos
--    * El número de palabras por párrafo
--    * El número de apariciones de la letra 'e' por párrafo
-- 
--    $ runhaskell Practica7.2.hs
--    (5,[105,158,46,64,52],[41,60,16,24,28])
-- ---------------------------------------------------------------------

cuentas fichero = do
  texto <- readFile fichero
  let lineas = [l | l <- (lines texto), length l > 1]
  let palabras = [length (words linea) | linea <- lineas]
  let es = [length es | linea <- lineas, let es = filter (=='e') linea]

  let resultado = (length lineas, palabras, es)
  putStrLn (show resultado)

main1 :: IO ()
main1 = cuentas "lorem_ipsum.txt"

-- main = main1

-- ---------------------------------------------------------------------
-- Ejercicio 2. Adaptar el ejercicio anterior para que podamos
-- pasarle al main como argumento el nombre del archivo a leer
-- (Descargue los ficheros: 
-- http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt
-- http://www.cs.us.es/cursos/pd/ejercicios/otro.txt
-- )
-- Si no recibimos archivo, debemos tratar el del ejercicio 1
-- 
--    $ runhaskell Practica7.2.hs
--    (5,[105,158,46,64,52],[79,108,38,43,30])
--
--    $ runhaskell Practica7.2.hs otro.txt
--    (8,[86,89,65,103,97,100,95,112],[62,57,50,69,63,60,69,65])
-- ---------------------------------------------------------------------

main2 :: IO ()
main2 = do
  args <- getArgs
  if length args > 0 then do
    let fichero = head args
    cuentas fichero
  else do 
    putStrLn "Debe indicar el nombre de archivo."

  return ()
  
--main = main2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Adaptar el ejercicio anterior para que trate el posible
-- error de lectura del fichero, como hemos visto en el tema.
--
--    $ runhaskell Practica7.2.hs
--    (5,[105,158,46,64,52],[79,108,38,43,30])
--
--    $ runhaskell Practica7.2.hs otro.txt
--    (8,[86,89,65,103,97,100,95,112],[62,57,50,69,63,60,69,65])
--
--    $ runhaskell Practica7.2.hs inexistente.txt
--    inexistente.txt: openFile: does not exist (No such file or directory)
--    "El fichero no existe"
-- ---------------------------------------------------------------------

main3 :: IO ()
main3 = do
  args <- getArgs
  if length args > 0 then do
    let fichero = head args
    existe <- doesFileExist fichero
    if existe then
      cuentas fichero
    else
      putStrLn ("El archivo " ++ fichero ++ " no existe.")
  else do 
    putStrLn "Debe indicar el nombre de archivo."

  return ()

-- main = main3

-- ---------------------------------------------------------------------
-- Ejercicio 4. Procesar el archivo CSV "clima.csv" 
-- descárgalo de http://www.cs.us.es/cursos/pd/ejercicios/clima.csv) 
-- y devolver los nombres de sus atributos.
-- Por ejemplo:
--    ghci> main
--    Cielo
--    Temperatura
--    Humedad
--    Viento
--    JugarTenis
-- ---------------------------------------------------------------------

main4 :: IO ()
main4 = do
  contenido <- parseCSVFromFile "clima.csv"
  let filas = case contenido of
             Right filas -> filas
             _ -> []
  sequence_ [putStrLn campo | campo <- head filas]
  
-- main = main4

-- ---------------------------------------------------------------------
-- Ejercicio 5. Procesar el archivo CSV "clima.csv" 
-- (descárgalo de http://www.cs.us.es/cursos/pd/ejercicios/clima.csv) 
-- y devolver la frecuencia de cada atributo.
-- Salida esperada:
-- Cielo
-- -----
-- Soleado: 0.357
-- Nublado: 0.286
-- Lluvia: 0.357
-- Temperatura
-- -----------
-- Baja: 0.286
-- Alta: 0.286
-- Suave: 0.429
-- Humedad
-- -------
-- Normal: 0.500
-- Alta: 0.500
-- Viento
-- ------
-- Debil: 0.571
-- Fuerte: 0.429
-- JugarTenis
-- ----------
-- Si: 0.643
-- No: 0.357
-- ---------------------------------------------------------------------

unicos :: (Eq a) => [a] -> [a]
unicos = foldr (\x acc -> if elem x acc then acc else x:acc ) []

traspuesta :: [[a]] -> [[a]]
traspuesta registros = [[(registros!!f)!!c | f <-[0..(length registros)-1]]  | c <- [0..(length (head registros))-1]]

frecuencia :: (Eq a) => a -> [a] -> Float
frecuencia valor valores = sum [1 | x <- valores, x == valor] / (fromIntegral (length valores))

mostrar_frecuencia valor valores = do
  putStr (valor ++ ": ")
  let f = frecuencia valor valores
  printf "%.3f\n" f

frecuencias campo valores = do
  putStrLn campo
  putStrLn (take (length campo) (repeat '-'))
  sequence_ [mostrar_frecuencia v  valores | v <- (unicos valores)]
  putStrLn ""

main5 :: IO ()
main5 = do
  contenido <- parseCSVFromFile "clima.csv"
  let filas = case contenido of
             Right filas -> filas
             _ -> []

  let cabecera = head filas
  let registros = tail filas
  sequence_ [frecuencias campo valores | (campo, valores) <- zip cabecera  (traspuesta registros)]

--main = main5



