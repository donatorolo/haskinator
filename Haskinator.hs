module Haskinator (main) where

import Oraculo
import Data.Maybe

-- ******************* Funciones Auxiliares al Cliente **************

-- Toma el oraculo viejo y una nueva pregunta y se la agrega al final en la afirmacion positiva

-- original -> nva pregunta -> cadena -> Actualizado

actualizarOraculo:: (Oraculo a) -> (Oraculo a) -> [(String,Bool)] -> (Oraculo a)
actualizarOraculo _  nuevaP [] = nuevaP
actualizarOraculo (Pregunta s p n)  nuevaP (x:xs) 
  | (snd x) == True = crearPregunta s (actualizarOraculo p nuevaP xs) n
  | otherwise = crearPregunta  s p (actualizarOraculo n nuevaP xs)

 
 
-- Funcion que agrega una nueva pregunta con su respuesta a un oraculo.
-- original -> vieja 
mejorarOraculo:: (Maybe (Oraculo a)) -> String -> IO () 
mejorarOraculo Nothing _ = putStrLn "El Oraculo no ha sido iniciado."
mejorarOraculo (Just (x)) vieja = do
  putStrLn "Por favor inquique la respuesta correcta: "
  resp <- getLine
  putStrLn "Por favor indique una pregunta que la distinga de la prediccion hecha:"
  preg <- getLine
  putStrLn "Muchas Gracias!" 
  menu (Just(actualizarOraculo x (crearPregunta preg (crearPrediccion resp) (crearPrediccion vieja)) 
                                 (fromJust (obtenerCadena x vieja))))  
  
temporal:: (Maybe[(String, Bool)]) -> [(String, Bool)]
temporal (Just(x)) = x
 -- *********************** Funciones del Cliente ********************

-- Funcion Predecir 
-- OraculoActual -> 
predecir:: (Maybe(Oraculo a)) ->  (Maybe(Oraculo a)) -> IO ()

predecir Nothing _ = putStrLn "Error: El Oraculo esta vacio"

predecir e@(Just(Pregunta s p n)) x = do
    putStrLn s
    respuesta <- getLine
    case respuesta of
      "si" -> predecir (Just p) x
      "no" -> predecir (Just n) x
      _    -> do 
        putStrLn "La respuesta debe ser si o no"
        predecir e x
   
predecir e@(Just(Prediccion s)) x = do
  putStrLn s
  putStrLn "¿Es esta prediccion acertada? (si/no):"
  respuesta <- getLine
  case respuesta of
   "si" -> putStrLn "Muchas Gracias"
   "no" -> mejorarOraculo x s
   _    -> putStrLn "La respuesta debe ser si o no"
  
    
-- *********************** Funciones de Menu ************************

-- Funcion que valida una opcion del menu
opcionValida :: Integer -> Bool
opcionValida x = (1 <= x) && (x <= 6)

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

-- Funcion  que da un mensaje de error del rango de opciones validas
errorOpcion:: IO ()
errorOpcion = putStrLn "\n Opcion Invalida. Solo enteros entre 1 y 6 \n"
 
-- Ejecuta la opcion que fue pasada como parametro (dada por el usuario)
ejecutar :: Integer -> (Maybe(Oraculo a)) -> IO ()
ejecutar seleccion x
  | seleccion == 1 = menu Nothing
  | seleccion == 2 = predecir x x 
  | seleccion == 3 = putStrLn "vamos para la opcion 3"
  | seleccion == 4 = putStrLn "vamos para la opcion 4"
  | seleccion == 5 = putStrLn "vamos para la opcion 5"
  | seleccion == 6 = putStrLn "vamos para la opcion 6"
  
-- Funcion que presenta el menu de opciones y ejecuta la que diga el usuario  
menu:: (Maybe(Oraculo a)) -> IO ()
menu x = do
  putStr $ "ORACULO: " ++ show x
  printOpciones
  seleccion <- readLn
  case opcionValida seleccion of
       True -> ejecutar seleccion x
       False -> errorOpcion
       
  menu x
  
  
pregunta1 = Pregunta "Es Ireal" pregunta2 pregunta3
pregunta2 = Pregunta "Es Masculino" hoja1 hoja2
pregunta3 = Pregunta "Es Buena Gente" hoja3 hoja4
 
hoja1 = Prediccion "MICKEY"
hoja2 = Prediccion "MINNIE"
hoja3 = Prediccion "CHAVEZ"
hoja4 = Prediccion "OBAMA"

main = do
  menu $ Just pregunta1