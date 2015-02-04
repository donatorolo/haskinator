--------------------------------------------------------------------------------
-- Haskinator.hs                                                              --
--                                                                            --
-- Autor:  Carlos Aponte 09-10041                                             --
--         Donato Rolo   10-10640                                             --
--------------------------------------------------------------------------------


module Main where

import Oraculo
import Data.Maybe
import System.IO
import System.Directory(doesFileExist)


--------------------------------------------------------------------------------
--                     FUNCIONES AUXILIARES AL CLIENTE                        --
--------------------------------------------------------------------------------

-- | actualizarOraculo: Esta funcion recibe como parametros dos 'Oraculos', uno 
--    representa el 'Oraculo' del Haskinator y el otro una Pregunta que se desea 
--    agregar. Así mismo, recibe una lista de tuplas (String, Bool) toma el 
--    'Oraculo' viejo y una nueva pregunta y se la agrega al final en la 
--    afirmación positiva.
actualizarOraculo:: (Oraculo) -> (Oraculo) -> [(String,Bool)] -> (Oraculo)
actualizarOraculo _  nuevaP [] = nuevaP
actualizarOraculo (Pregunta s p n)  nuevaP (x:xs) 
  | (snd x) == True = crearPregunta s (actualizarOraculo p nuevaP xs) n
  | otherwise = crearPregunta  s p (actualizarOraculo n nuevaP xs)

-------------------------------------------------------------------------------- 
 
-- | mejorarOraculo: Esta función recibe como parametros un 'Maybe Oraculo' que 
--    representa el 'Oraculo' actual del Haskinator, la predicción anterior 
--    en forma de String y una lista de (String,Bool) que representa el camino 
--    hasta dicha predicción. La función le solicita al usuario la información 
--    para crear la pregunta que diferencia la predicción vieja de la respuesta 
--    correcta, y seguidamente crear un 'Oraculo' de tipo Prediccion. Finalmente 
--    llama a la función actualizar 'Oraculo' para agregar dicha pregunta al 
--    'Oraculo'.
mejorarOraculo:: (Maybe (Oraculo)) -> String -> [(String, Bool)] -> IO() 
mejorarOraculo Nothing _ _= putStrLn "El Oraculo no ha sido iniciado."
mejorarOraculo (Just (x)) vieja camino = do
  putStrLn "\nPor favor inquique la respuesta correcta: "
  resp <- getLine
  putStrLn 
        "Por favor indique una pregunta que la distinga de la prediccion hecha:"
  preg <- getLine
  putStrLn "Muchas Gracias!"
  menu (Just(actualizarOraculo x (crearPregunta preg (crearPrediccion resp) 
                                  (crearPrediccion vieja)) 
                                  (camino)))  
  
--------------------------------------------------------------------------------

-- | calcularAncestroComun: Esta función recibe un 'Maybe (Oraculo)' y dos 
--      predicciones en forma de Strings. La función procede a calcular el 
--      camino más positivo a cada 'Prediccion' y a calcular el ancestro común 
--      más temprano de ambos camino; en caso de que alguna 'Predicción' no este 
--      en el 'Oraculo' se le notifica al usuario y se pide nuevamente. Ya para 
--      finalizar la función imprime por pantalla el ancestro común (que a su 
--      vez es la pregunta crucial que separa) esas dos predicciones.
calcularAncestroComun:: Maybe (Oraculo) -> String -> String -> IO ()
calcularAncestroComun oraculo p1 p2 = do 
  let camino1 = obtenerCadena (fromJust(oraculo)) p1
  let camino2 = obtenerCadena (fromJust(oraculo)) p2
  crucial camino1 camino2 (fromJust oraculo)
  menu oraculo
  where
    crucial c1 c2 x
      | c1 == Nothing = putStrLn 
                        "La primera prediccion indicada no esta en el Oraculo."
      | c2 == Nothing = putStrLn 
                        "La segunda prediccion indicada no esta en el Oraculo."
      | otherwise = do
        putStrLn "La pregunta crucial entre las predicciones es: "
        putStr "  --> " 
        putStrLn $ fst $ head  [ x | x <- (fromJust c1) , not 
                               $ elem x (fromJust c2)]  
 
--------------------------------------------------------------------------------
--                         FUNCIONES DEL CLIENTE                              --
--------------------------------------------------------------------------------

-- | persistir: Esta función recibe como parametro un 'Maybe Oraculo' y procede 
--      a solicitarle al usuario el nombre de un archivo donde se guardará dicho 
---     'Oraculo'.
persistir:: Maybe Oraculo -> IO ()
persistir oraculo = do
  putStr "\ESC[2J"
  putStrLn "Indique el archivo donde se guardará el Oráculo: "
  archivo <- getLine
  outh <- openFile archivo WriteMode
  hPutStrLn outh (show $ fromJust oraculo)
  hClose outh
  putStrLn $ "Guardado del Oraculo en el archivo: " ++ archivo ++ " EXITOSO!."
  menu oraculo

--------------------------------------------------------------------------------

-- | cargar: Esta función pide al usuario un nombre de archivo en donde se 
--      encuentra un 'Oraculo' el cual se procede a leer y a cargar, volviéndose
--      el nuevo 'Oraculo' del Haskinator.
cargar:: IO ()
cargar = do
  putStr "\ESC[2J"
  putStrLn "Indique el archivo desde donde se cargará el Oráculo: "
  archivo <- getLine
  exist <- doesFileExist archivo
  if exist then do 
        contenido <- readFile archivo
        menu $ Just $ read contenido
        else do
          putStrLn "\nEl archivo indicado no existe, intentelo nuevamente...\n"
          cargar

--------------------------------------------------------------------------------  
  
-- | estadisticas: Esta función recibe como parámetro un 'Maybe Oraculo' y le 
--      pide al usuario una cadena de caracteres correspondiente a una 
--      'Prediccion'. Devuelve como resultado las estadísticas de mínimo, máximo
--      y promedio de preguntas necesarias para llegar a esa 'Prediccion'; en 
--      caso de que la 'Prediccion' no este contenida en el 'Oraculo', lo 
--      notifica y pide una nueva 'Prediccion'.
estadisticas:: Maybe Oraculo -> IO ()
estadisticas x = do
   putStr "\ESC[2J"
   putStrLn "Indique el nombre de la prediccion que desea buscar: "
   buscar <- getLine
   imprimir(obtenerEstadisticas (fromJust x) buscar)
   menu x
   where
     imprimir (x,y,z) 
       | x == 0  = putStrLn 
                   "La prediccion solicitada no se encuentra en el Oraculo"
       | otherwise = do
     putStr "\nCantidad Minima de Preguntas a Realizar: "
     print x
     putStr "Cantidad Maxima de Preguntas a Realizar:  "
     print y
     putStr "Cantidad Promedio de Preguntas a Realizar: "
     print z
     putStr "\n"

--------------------------------------------------------------------------------

-- | predecir: Esta función recibe como parámetros dos 'Maybe Oraculo', el 
--      primero sirve para mantener el 'Oraculo' original, y el segundo para ir 
--      recorriendo el árbol del 'Oraculo'. Así mismo recibe una lista de tuplas
--      (String, Bool) que representan el camino recorrido hasta la 'Prediccion'
--      final. Por pantalla va consultando al usuario sobre como recorrer el 
--      'Oraculo' hasta que llega a una 'Prediccion', en ese momento si la 
--      'Prediccion' fue correcta se regresa al menú, en caso contrario se le 
--      pide al usuario la información necesaria para corregir y actualizar el 
--      'Oraculo'.
predecir::Maybe Oraculo ->  Maybe Oraculo ->  [(String,Bool)] -> IO ()

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

-- | preguntaCrucial: Esta función recibe como parámetros el 'Maybe Oraculo' con
--      el que se esta trabajando actualmente y procede a consultarle al usuario
--      a que 'Prediccion' desea buscarles la pregunta crucial que las separa.
--      Seguidamente procede a buscar el ancestro común de esas 'Predicciones' y 
--      notificárselo al usuario.
preguntaCrucial:: Maybe Oraculo -> IO ()
preguntaCrucial x = do
  putStr "\ESC[2J"
  putStrLn "Indique Primera Prediccion:"
  pred1 <- getLine
  putStrLn "Indique la Segunda Prediccion:"
  pred2 <- getLine
  calcularAncestroComun x pred1 pred2  
  menu x

--------------------------------------------------------------------------------
--                             FUNCIONES MENÚ                                 --
--------------------------------------------------------------------------------

-- | opcionValida: Esta función recibe un entero que representa la opción 
--      seleccionada por el usuario y valida que sea manejable por el menú.
opcionValida :: Integer -> Bool
opcionValida x = (1 <= x) && (x <= 6)

--------------------------------------------------------------------------------

-- | printOpciones: Esta funcion imprime por pantalla las opciones disponibles 
--      en el menú del Haskinator.
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

-- | errorOpcion: Esta función que da un mensaje de error del rango de opciones 
--       válidas
errorOpcion:: IO ()
errorOpcion = putStrLn "\n Opcion Invalida. Solo enteros entre 1 y 6 \n"
 
--------------------------------------------------------------------------------

-- | ejecutar: Esta función que recibe un entero el cual representa una opción
--       válida del menú, y procede a ejecutarla.
ejecutar :: Integer -> Maybe Oraculo -> IO ()
ejecutar seleccion x
  | seleccion == 1 = menu Nothing
  | seleccion == 2 = predecir x x []
  | seleccion == 3 = persistir x
  | seleccion == 4 = cargar
  | seleccion == 5 = preguntaCrucial x
  | seleccion == 6 = estadisticas x

--------------------------------------------------------------------------------

-- | menu: Esta función que se encarga de mostrarle al usuario las opciones del 
--      Haskinator, verificar la validez y ejecuta las solicitudes del usuario.
menu:: Maybe Oraculo -> IO ()
menu x = do
 
  printOpciones
  seleccion <- readLn
  case opcionValida seleccion of
       True -> ejecutar seleccion x
       False -> errorOpcion
       
  menu x

--------------------------------------------------------------------------------
--                                   MAIN                                     --
--------------------------------------------------------------------------------

-- | main: Esta es la función principal del programa y única visible desde 
--      afuera del Haskinator. Esta función inicia llamando al main con 
--      'Maybe Oraculo' "vacio" representado por 'Nothing'.
main = menu Nothing

--------------------------------------------------------------------------------