module DefinicionesNim
       (Nim, inicio, estrellasRestantes, jugador, eliminaEstrellas)
       where

data Jugador = J Bool
instance Show Jugador where
  show j = "Tiene el turno el " ++ showJAux j ++ " jugador" 

showJAux :: Jugador -> String
showJAux (J True) = "primer"
showJAux _ = "segundo"

data Nim = Uno Int | Dos Int
instance Show Nim where
  show (Uno n) = showEAux n ++ show (J True)
  show (Dos n) = showEAux n ++ show (J False)

showEAux n | n == 0 = "No quedan estrellas\n"
           | n == 1 = "Queda 1 estrella\n"
           | otherwise = "Quedan " ++ show n ++ " estrellas\n"

inicio :: Int -> Nim
inicio n
  | n `elem` [1..20] = Uno n
  | otherwise =
    error "inicio: La cantidad de estrellas inicial tiene que estar
entre 1 y 20." 

estrellasRestantes :: Nim -> Int
estrellasRestantes (Uno n)
  | n >= 0 = n
  | otherwise = error "estrellasRestantes: no pueden faltar estrellas"
estrellasRestantes (Dos n) 
  | n >= 0 = n
  | otherwise = error "estrellasRestantes: no pueden faltar estrellas"

jugador :: Nim -> Int
jugador (Uno _) = 1
jugador _ = 2

eliminaEstrellas :: Nim -> Int -> Nim
eliminaEstrellas _ c
  | not (c `elem` [1..3]) =
    error "eliminaEstrellas: sólo se pueden coger 1, 2 ó 3 estrellas"
eliminaEstrellas (Uno n) c
  | n >= c = (Dos (n-c))
  | otherwise =
    error "eliminaEstrellas: No hay suficientes estrellas."                
eliminaEstrellas (Dos n) c
  | n >= c = (Uno (n-c))
  | otherwise =
    error "eliminaEstrellas: No hay suficientes estrellas."
                

