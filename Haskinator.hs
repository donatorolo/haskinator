module Haskinator (main) where

import Oraculo
import Data.Maybe
import System.IO


--------------------------------------------------------------------------------
--                     FUNCIONES AUXILIARES AL CLIENTE                        --
--------------------------------------------------------------------------------

-- Toma el oraculo viejo y una nueva pregunta y se la agrega al final en la 
-- afirmacion positiva 
-- original -> nva pregunta -> cadena -> Actualizado

actualizarOraculo:: (Oraculo a) -> (Oraculo a) -> [(String,Bool)] -> (Oraculo a)
actualizarOraculo _  nuevaP [] = nuevaP
actualizarOraculo (Pregunta s p n)  nuevaP (x:xs) 
  | (snd x) == True = crearPregunta s (actualizarOraculo p nuevaP xs) n
  | otherwise = crearPregunta  s p (actualizarOraculo n nuevaP xs)

-------------------------------------------------------------------------------- 
 
-- Funcion que agrega una nueva pregunta con su respuesta a un oraculo.
-- original -> vieja 
mejorarOraculo:: (Maybe (Oraculo a)) -> String -> [(String, Bool)] -> IO () 
mejorarOraculo Nothing _ _= putStrLn "El Oraculo no ha sido iniciado."
mejorarOraculo (Just (x)) vieja camino = do
  putStrLn "Por favor inquique la respuesta correcta: "
  resp <- getLine
  putStrLn "Por favor indique una pregunta que la distinga de la prediccion hecha:"
  preg <- getLine
  putStrLn "Muchas Gracias!" 
  menu (Just(actualizarOraculo x (crearPregunta preg (crearPrediccion resp) (crearPrediccion vieja)) 
                                 (camino)))  
  
--------------------------------------------------------------------------------

-- calcularAncestroComun
calcularAncestroComun:: Maybe (Oraculo a) -> String -> String -> IO ()
calcularAncestroComun oraculo p1 p2 = do 
  let camino1 = obtenerCadena (fromJust(oraculo)) p1
  let camino2 = obtenerCadena (fromJust(oraculo)) p2
  crucial camino1 camino2 (fromJust oraculo)
  menu oraculo
  where
    crucial c1 c2 x
      | c1 == Nothing = putStrLn "La primera prediccion indicada no esta en el Oraculo."
      | c2 == Nothing = putStrLn "La segunda prediccion indicada no esta en el Oraculo."
      | otherwise = do
        putStrLn "La pregunta crucial entre las predicciones es: "
        putStr "  --> " 
        putStrLn $ fst $ head  [ x | x <- (fromJust c1) , not $ elem x (fromJust c2)]  
 

--------------------------------------------------------------------------------
--                         FUNCIONES DEL CLIENTE                              --
--------------------------------------------------------------------------------


-- Funcion Persistir (Guardar): se debe almacena la informacion del oraculo construido en el archivo
-- suministrado

persistir:: Maybe(Oraculo a) -> IO ()
persistir oraculo = do
  putStrLn "Indique el archivo donde se guardará el Oráculo: "
  archivo <- getLine
  outh <- openFile archivo WriteMode
  hPutStrLn outh (show $ fromJust oraculo)
  hClose outh
  putStrLn $ "Guardado del Oraculo en el archivo: " ++ archivo ++ " EXITOSO!."
  menu oraculo

--------------------------------------------------------------------------------

-- Funcion Cargar: Si esta opcion es seleccionada, se debe pedir un nombre de archivo al usuario y luego
-- se debe cargarar la informacion al oraculo desde el archivo suministrado

-- cargar:: IO ()
-- cargar = do
--   putStrLn "Indique el archivo desde donde se cargará el Oráculo: "
--   archivo <- getLine
--   contenido <- readFile archivo

  -- debo llamar al read peeero el read no lo se instanciar

--------------------------------------------------------------------------------  
  
 --Estadisticas: 
estadisticas:: Maybe(Oraculo a) -> IO ()
estadisticas x = do
   putStrLn "Indique el nombre de la prediccion que desea buscar: "
   buscar <- getLine
   imprimir(obtenerEstadisticas (fromJust x) buscar)
   menu x
   where
     imprimir (x,y,z) 
       | x == 0  = putStrLn "La prediccion solicitada no se encuentra en el Oraculo"
       | otherwise = do
   putStr "\nCantidad Minima de Preguntas a Realizar: "
   print x
   putStr "Cantidad Maxima de Preguntas a Realizar:  "
   print y
   putStr "Cantidad Promedio de Preguntas a Realizar: "
   print z
   putStr "\n"

--------------------------------------------------------------------------------

-- Funcion Predecir: 
-- OraculoActual -> 
predecir:: (Maybe(Oraculo a)) ->  (Maybe(Oraculo a)) ->  [(String,Bool)] -> IO ()

predecir Nothing _ _ = putStrLn "Error: El Oraculo esta vacio"

predecir e@(Just(Pregunta s p n)) x camino= do
    putStrLn s
    respuesta <- getLine
    case respuesta of
      "si" -> predecir (Just p) x ((s,True):camino)
      "no" -> predecir (Just n) x ((s,False):camino)
      _    -> do 
        putStrLn "La respuesta debe ser si o no"
        predecir e x camino
   
predecir e@(Just(Prediccion s)) x camino= do
  putStrLn s
  putStrLn "¿Es esta prediccion acertada? (si/no):"
  respuesta <- getLine
  case respuesta of
   "si" -> putStrLn "Muchas Gracias"
   "no" -> mejorarOraculo x s camino
   _    -> putStrLn "La respuesta debe ser si o no"
  
--------------------------------------------------------------------------------

--   Pregunta Crucial
preguntaCrucial:: Maybe(Oraculo a) -> IO ()
preguntaCrucial x = do
  putStrLn "Indique Primera Prediccion:"
  pred1 <- getLine
  putStrLn "Indique la Segunda Prediccion:"
  pred2 <- getLine
  calcularAncestroComun x pred1 pred2  
  menu x
  
  
--------------------------------------------------------------------------------  
    
-- *********************** Funciones de Menu ***********************

--------------------------------------------------------------------------------
--                             FUNCION MENU                                   --
--------------------------------------------------------------------------------

-- Funcion que valida una opcion del menu
opcionValida :: Integer -> Bool
opcionValida x = (1 <= x) && (x <= 6)

--------------------------------------------------------------------------------

-- Funcion que muestra todas las opciones que tiene el usuario
printOpciones :: IO ()
printOpciones = do
  putStrLn  "\n\t 1) Crear Nuevo Oraculo"
  putStrLn  "\t 2) Predecir" 
  putStrLn  "\t 3) Persistir"
  putStrLn  "\t 4) Cargar"
  putStrLn  "\t 5) Consultar Pregunta Crucial" 
  putStrLn  "\t 6) Consultar Estadisticas \n"
  putStrLn  "\t Seleccione una opcion (1..6):"

--------------------------------------------------------------------------------

-- Funcion  que da un mensaje de error del rango de opciones validas
errorOpcion:: IO ()
errorOpcion = putStrLn "\n Opcion Invalida. Solo enteros entre 1 y 6 \n"
 
--------------------------------------------------------------------------------


-- Ejecuta la opcion que fue pasada como parametro (dada por el usuario)
ejecutar :: Integer -> (Maybe(Oraculo a)) -> IO ()
ejecutar seleccion x
  | seleccion == 1 = menu Nothing
  | seleccion == 2 = predecir x x []
  | seleccion == 3 = persistir x
  | seleccion == 4 = putStrLn "vamos para la opcion 4"
  | seleccion == 5 = preguntaCrucial x
  | seleccion == 6 = estadisticas x

--------------------------------------------------------------------------------

-- Funcion que presenta el menu de opciones y ejecuta la que diga el usuario  
menu:: (Maybe(Oraculo a)) -> IO ()
menu x = do
  printOpciones
  seleccion <- readLn
  case opcionValida seleccion of
       True -> ejecutar seleccion x
       False -> errorOpcion
       
  menu x
  
--------------------------------------------------------------------------------
--                             ORACULOS PRUEBA                                --
--------------------------------------------------------------------------------

   
pregunta1 = Pregunta "Es Ireal" pregunta2 pregunta3
pregunta2 = Pregunta "Es Masculino" hoja1 hoja2
pregunta3 = Pregunta "Es Buena Gente" hoja3 hoja4
 
hoja1 = Prediccion "MICKEY"
hoja2 = Prediccion "MINNIE"
hoja3 = Prediccion "CHAVEZ"
hoja4 = Prediccion "OBAMA"

-- Oraculo de numeros, arbol del 1 al 15
h15 = crearPrediccion "15"
h14 = crearPrediccion "10"
h13 = crearPrediccion "11"
h12 = crearPrediccion "10"
h10 = crearPrediccion "10"
h9 = crearPrediccion "9"
h8 = crearPrediccion "8"
h5 = crearPrediccion "5"

h11 = crearPregunta "11" h14 h15
h7 = crearPregunta "7" h12 h13
h6 = crearPregunta "6" h10 h11
h4 = crearPregunta "4" h8 h9
h3 = crearPregunta "3" h6 h7
h2 = crearPregunta "2" h4 h5
h1 = crearPregunta "1" h2 h3


--------------------------------------------------------------------------------
--                                   MAIN                                     --
--------------------------------------------------------------------------------

main = do
  menu $ Just pregunta1