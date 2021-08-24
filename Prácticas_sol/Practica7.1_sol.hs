-- PD: entrada/salida
-- El juego del nim y las funciones de entrada/salida. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- En el juego del nim el tablero tiene 5 filas numeradas de estrellas,
-- cuyo contenido inicial es el siguiente 
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
-- Dos jugadores retiran por turno una o más estrellas de una fila. El
-- ganador es el jugador que retire la última estrella. En este
-- ejercicio se va implementar el juego del Nim para practicar con las
-- funciones de entrada y salida.
-- Nota: El juego debe de ejecutarse en una consola, no en la shell de
-- emacs. 

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char
import Text.Printf
import System.IO
import System.Directory

-- ---------------------------------------------------------------------
-- § Representación                                                   --
-- ---------------------------------------------------------------------
 
-- El tablero se representará como una lista de números indicando el
-- número de estrellas de cada fila. Con esta representación, el tablero
-- inicial es [5,4,3,2,1]. 

-- Representación del tablero.
type Tablero = [Int]

-- inicial es el tablero al principio del juego.
inicial ::  Tablero
inicial =  [5,4,3,2,1]

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    finalizado :: Tablero -> Bool
-- tal que (finalizado t) se verifica si t es el tablero de un juego
-- finalizado; es decir, sin estrellas. Por ejemplo,
--    finalizado [0,0,0,0,0]  ==  True
--    finalizado [1,3,0,0,1]  ==  False
-- ---------------------------------------------------------------------

finalizado :: Tablero -> Bool
finalizado t = (sum t) == 0

-- ---------------------------------------------------------------------
-- Ejecicio 2.2. Definir la función
--    valida :: Tablero -> Int -> Int -> Bool
-- tal que (valida t f n) se verifica si se puede coger n estrellas en
-- la fila f del tablero t y n es mayor o igual que 1. Por ejemplo,
--    valida [4,3,2,1,0] 2 3  ==  True
--    valida [4,3,2,1,0] 2 4  ==  False
--    valida [4,3,2,1,0] 2 2  ==  True
--    valida [4,3,2,1,0] 2 0  ==  False
-- ---------------------------------------------------------------------

valida :: Tablero -> Int -> Int -> Bool
valida t f n = (n >= 1) && (t!!(f-1) >= n)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    jugada :: Tablero -> Int -> Int -> Tablero
-- tal que (jugada t f n) es el tablero obtenido a partir de t
-- eliminando n estrellas de la fila f. Por ejemplo,
--    jugada [4,3,2,1,0] 2 1  ==  [4,2,2,1,0]
-- ---------------------------------------------------------------------

jugada :: Tablero -> Int -> Int -> Tablero
jugada t f n = [if i == f then  x - n else x | (x, i) <- zip t [1,2..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la acción
--    nuevaLinea :: IO ()
-- que consiste en escribir una nueva línea. Por ejemplo,
--    ghci> nuevaLinea
--    
--    ghci> 
-- ---------------------------------------------------------------------

nuevaLinea :: IO ()
nuevaLinea = putChar '\n'

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    estrellas :: Int -> String
-- tal que (estrellas n) es la cadena formada con n estrellas. Por
-- ejemplo, 
--    ghci> estrellas 3
--    "* * * "
-- ---------------------------------------------------------------------

estrellas :: Int -> String
estrellas n = unwords ["*" | _ <- [1..n]]
                              
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la acción
--    escribeFila :: Int -> Int -> IO ()
-- tal que (escribeFila f n) escribe en la fila f n estrellas. Por
-- ejemplo, 
--    ghci> escribeFila 2 3
--    2: * * *
-- ---------------------------------------------------------------------


escribeFila :: Int -> Int -> IO ()
escribeFila f n = printf "%d: %s\n" f (estrellas n)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la acción
--    escribeTablero :: Tablero -> IO ()
-- tal que (escribeTablero t) escribe el tablero t. Por
-- ejemplo,
--    ghci> escribeTablero [3,4,1,0,1]
--    1: * * * 
--    2: * * * * 
--    3: * 
--    4: 
--    5: * 
-- ---------------------------------------------------------------------

escribeTablero :: Tablero -> IO ()
escribeTablero t = sequence_ [escribeFila i n | (n, i) <- zip t [1,2..]]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la acción
--    leeDigito :: String -> IO Int
-- tal que (leeDigito c) escribe una nueva línea con l cadena "prueba",
-- lee un carácter y comprueba que es un dígito. Además, si el carácter
-- leido es un dígito entonces devuelve el entero correspondiente y si
-- no lo es entonces escribe el mensaje "Entrada incorrecta" y vuelve a
-- leer otro carácter. Por ejemplo,  
--    ghci> leeDigito "prueba "
--    prueba 3
--    3
--    ghci> leeDigito "prueba "
--    prueba c
--    ERROR: Entrada incorrecta
--    prueba 3
--    3
-- ---------------------------------------------------------------------

leeDigito :: String -> IO Int
leeDigito c = do
    putStr c
    x <- getChar
    if isDigit x then
        return (read [x])
    else do
        putStrLn "\nEntrada incorrecta (se esperaba un dígito)"
        d <- leeDigito c
        return d

-- ---------------------------------------------------------------------
-- Ejercicio 9. Los jugadores se representan por los números 1 y 2.
-- Definir la función 
--    siguiente :: Int -> Int
-- tal que (siguiente j) es el jugador siguiente de j. 
-- ---------------------------------------------------------------------

siguiente :: Int -> Int
siguiente j = (mod j 2) + 1

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la acción
--    juego :: Tablero -> Int -> IO ()
-- tal que (juego t j) es el juego a partir del tablero t y el turno del
-- jugador j. Por ejemplo,
--    ghci> juego [0,1,0,1,0] 2
--    
--    1: 
--    2: * 
--    3: 
--    4: * 
--    5: 
--    
--    J 2
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: 
--    
--    J 1
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

limpiar = putStr "\ESC[2J"

selecciona :: Tablero -> IO (Int, Int)
selecciona t = do
    f <- leeDigito "Elije una fila: "
    putChar '\n'
    n <- leeDigito "Elige cuantas estrellas retiras: "
    putChar '\n'
    if valida t f n then do       
        return (f, n)
    else do
        putStrLn "Moviento no válido. Inténtalo de nuevo.\n"
        r <- (selecciona t)
        return r


juego :: Tablero -> Int -> IO ()
juego t j = do
    limpiar
    escribeTablero t
    putChar '\n'
    putStr "Turno para el jugador "
    putStrLn (show j)
    -- pedimos la jugada
    (f, n) <- selecciona t
    -- ejecutamos la jugada
    let nt = jugada t f n
    -- comprobamos is ha ganado
    if finalizado nt then
        putStrLn ("El jugado " ++ (show j) ++ " ha ganado !!!")
    else do
        juego nt (siguiente j)
        return ()




-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la acción
--    nim :: IO ()
-- consistente en una partida del nim. Por ejemplo, se puede desarrollar
-- en una consola (no en la shell de emacs) como sigue
--    ghci> nim
--    
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 3
--    Elige cuantas estrellas retiras: 3
--    
--    1: * 
--    2: * * * * 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: * 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: * 
--    
--    J 1
--    Elige una fila: 5
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

nim :: IO ()
nim = juego inicial 1

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función principal para poder compilar el
-- el fichero. Compila el fichero y genera un ejecutable.
-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Ejercicio 13. Implementa un menú inicial que permita o bien comenzar
-- un juego nuevo, o bien cargar una partida desde un fichero (nombre indicado 
-- por el usuario). El formato del fichero debe ser:
--   jugador
--   n1 n2 n3 n4 n5
-- Es decir, la primera línea indicará el jugador que tenía el turno,
-- especificado por su número (1 o 2). La siguiente línea indica, con
-- separación de comas, el número de estrellas en cada fila.
-- ---------------------------------------------------------------------

split:: Char -> String -> [String]
split sep  = words . foldr (\x acc -> if x == sep then ' ':acc else x:acc) "" 

leerPartida :: IO (Int, [Int])
leerPartida = do
    putChar '\n'
    putStr "Indique el nombre de archivo: "
    f <- getLine
    existe <- doesFileExist f
    if existe then do
        contenido <- readFile f
        let lineas = lines contenido
        let jugador = read (head lineas) :: Int
        let tablero =  [(read x :: Int) | x <- split ',' (last lineas)]
        return (jugador, tablero)
    else 
        putStrLn "El archivo indicado no existe\n"
        leerPartida


nuevoJuego = do
    putStrLn "1. Nuevo Juego"
    putStrLn "2. Leer juego de archivo"
    putStr "Elija una opción: "
    o <- getChar
    getChar
    if o == '2' then do
        (j, t) <- leerPartida
        putChar '\n'
        juego t j
        
    else do
        putChar '\n'
        juego inicial 1

    return ()

main = do
    hSetBuffering stdout NoBuffering
    nuevoJuego
-- ---------------------------------------------------------------------
-- Ejercicio 14. Re-implementa las funciones necesarias para que, cuando
-- solicite una fila, si el usuario pulsa la letra 'q', el juego termine.
-- Antes de salir, debe preguntar si se quiere guardar partida, si se 
-- elige que 's', entonces debe preguntar por un nombre de fichero.
-- Finalmente, la partida se guarda con el formato indicado en el
-- ejercicio 13.
-- ---------------------------------------------------------------------
