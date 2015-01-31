-- oraculo.hs
-- Autor:  Carlos Aponte 09-10041
--         Donato Rolo   10-10640

module Oraculo 
    ( Oraculo(..)
    , crearPrediccion
    , crearPregunta
    , prediccion
    , pregunta
    , positivo
    , negativo
    , obtenerCadena
    , obtenerEstadisticas
    ) where

--------------------------------------------------------------------------------
--                                LIBRERÍAS                                   --
--------------------------------------------------------------------------------


import Data.Maybe
import Data.Function (on)
import Data.List (sortBy)

--------------------------------------------------------------------------------
--                             ORACULOS PRUEBA                                --
--------------------------------------------------------------------------------



-- Oraculo de numeros, arbol del 1 al 15
h15 = crearPrediccion "15"
h14 = crearPrediccion "10"
h13 = crearPrediccion "13"
h12 = crearPrediccion "10"
h10 = crearPrediccion "10"
h9 = crearPrediccion "9"
h8 = crearPrediccion "10"
h5 = crearPrediccion "5"

h11 = crearPregunta "11" h14 h15
h7 = crearPregunta "7" h12 h13
h6 = crearPregunta "6" h10 h11
h4 = crearPregunta "4" h8 h9
h3 = crearPregunta "3" h6 h7
h2 = crearPregunta "2" h4 h5
h1 = crearPregunta "1" h2 h3


pregunta1 = Pregunta "Esireal" pregunta2 pregunta3


pregunta2 = Pregunta "Esmasculino" hoja1 hoja2


pregunta3 = Pregunta "EsunHDP" hoja3 pregunta4

pregunta4 = Pregunta "EsunaRata" hoja1 hoja4

hoja1 = Prediccion "MICKEY"
hoja2 = Prediccion "MINNIE"
hoja3 = Prediccion "CHAVEZ"
hoja4 = Prediccion "OBAMA"

--------------------------------------------------------------------------------
--                             TIPOS DE DATOS                                 --
--------------------------------------------------------------------------------

-- Tipo Oraculo
--      Prediccion: 
--         Una cadena de caracteres con la predicción en cuestión.
--      Preguntas:
--         Una cadena de caracteres con la pregunta en cuestión
--         Un oráculo que corresponda a una respuesta positiva para la pregunta.
--         Un oráculo que corresponda a una respuesta negativa para la pregunta.

data Oraculo a = Prediccion String | Pregunta String (Oraculo a) (Oraculo a)
    deriving(Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
--                       FUNCIONES DE CONSTRUCCIÓN                            --
--------------------------------------------------------------------------------

-- crearPrediccion: Esta función recibe una cadena de caracteres, y devuelve un 
--                  'Oraculo' de tipo 'Prediccion'.
crearPrediccion :: String -> Oraculo a
crearPrediccion = Prediccion

--------------------------------------------------------------------------------

-- crearPregunta: Esta función recibe una cadena de caracteres y dos 'Oraculos'. 
--                Y devuelve un 'Oraculo' de tipo Pregunta con la cadena 
--                suministrada como pregunta, el primer 'Oraculo' como respuesta 
--                positiva y el segundo como negativa.
crearPregunta:: String -> Oraculo a -> Oraculo a -> Oraculo a
crearPregunta = Pregunta

--------------------------------------------------------------------------------
--                         FUNCIONES DE ACCESO                                --
--------------------------------------------------------------------------------

-- prediccion: Esta función recibe un 'Oraculo' y devuelve la cadena de 
--             caracteres, siempre y cuando sea una 'Prediccion'.
prediccion :: Oraculo a -> String
prediccion (Prediccion a) = a
prediccion _              = error 
                             "El elemento insertado no es del tipo 'Prediccion'"

--------------------------------------------------------------------------------

-- pregunta:  Esta función recibe un 'Oraculo' y devuelve la cadena de 
--            caracteres, siempre y cuando sea una 'Pregunta'.
pregunta :: Oraculo a -> String
pregunta (Pregunta a _ _) = a
pregunta _                = error 
                               "El elemento insertado no es del tipo 'Pregunta'"

--------------------------------------------------------------------------------

-- positivo:  Esta función recibe un 'Oraculo' y devuelve el 'Oraculo' que 
--            corresponde a una respuesta postiva, siempre y cuando sea 
--            una 'Pregunta'.
positivo :: Oraculo a -> Oraculo a
positivo (Pregunta _ a _) = a
positivo _                = error 
                               "El elemento insertado no es del tipo 'Pregunta'"

--------------------------------------------------------------------------------

-- negativo:  Esta función recibe un 'Oraculo' y devuelve el 'Oraculo' que 
--            corresponde a una respuesta negativa, siempre y cuando sea 
--            una 'Pregunta'.
negativo :: Oraculo a -> Oraculo a
negativo (Pregunta _ _ a) = a
negativo _                = error 
                               "El elemento insertado no es del tipo 'Pregunta'"

--------------------------------------------------------------------------------
--                       FUNCIONES DE MODIFICACIÓN                            --
--------------------------------------------------------------------------------

-- obtenerCadena: Esta función recibe un 'Oraculo' y una cadena de caracteres 
--                correspondiente a una 'Prediccion'. Devuelve un valor de tipo 
--                'Maybe'. Si la 'Prediccion' suministrada no pertenece al 
--                'Oraculo', retorna 'Nothing'. De lo contrario retorna 
--                'Just lista', donde 'lista' es una lista de tuplas 
--                (String, Bool) que corresponden a todas las preguntas que 
--                deben hacerse a partir de la raíz del 'Oraculo', para alcanzar
--                la 'Prediccion' suministrada y el valor de verdad (decisión 
--                positiva o negativa) de la misma.

obtenerCadena :: Oraculo a -> String -> Maybe [(String, Bool)]
obtenerCadena (Prediccion a) b 
    | a == b    = Just []
    | otherwise = Nothing
obtenerCadena (Pregunta a yes no) b = attach inYes $ if inYes then goYes 
                                                              else goNo
    where
        inYes  = goYes /= Nothing
        goYes  = obtenerCadena yes b
        goNo   = obtenerCadena no b
        attach r  Nothing  = Nothing
        attach r (Just xs) = Just $ (a, r) : xs

--------------------------------------------------------------------------------

--obtenerEstadisticas: Esta función recibe un 'Oraculo' y devuelve una 3-tupla
--                     con los siguientes datos: El mínimo, el máximo y 
--                     promedio de preguntas que el Oráculo necesita hacer para
--                     llegar a una predicción.

obtenerEstadisticas :: Oraculo a -> String -> (Int,Int,Double)
obtenerEstadisticas a b  
  | l == [] = (0,0,0) 
  | otherwise = (minimum l, maximum l,average l)
    where
        l = getList a b [] 0
        getList :: Oraculo a -> String -> [Int] -> Int -> [Int]
        getList (Pregunta a yes no) b list counter = list ++ goYes ++ goNo
            where
                goYes = getList yes b list (counter+1)
                goNo  = getList no b list (counter+1)
        getList (Prediccion a) b list counter
                | a == b    = counter:list
                |otherwise  = list
        average :: [Int] -> Double
        average l = (sum $ map fromIntegral l) / (fromIntegral $ length l)

--------------------------------------------------------------------------------
--                               INSTANCIAS                                   --
--------------------------------------------------------------------------------

---- 

--instance Show (Oraculo a) where
--    show a = help a 0
--        where
--            help (Prediccion a) n      = replicate (n*2) ' ' ++ "Prediccion " ++ 
--                                         a ++ "\n"
--            help (Pregunta a yes no) n = replicate (n*2) ' ' ++ "Pregunta " ++
--                                         a ++ "\n" ++ help yes (n+1) ++ 
--                                         help no (n+1)

              
---- instance Read (Oraculo a) where
----        readsPrec _ a = head $ crearOraculo $ reverse $ map auxiliar3 $ lines a






---- Sea a el string de un oraculo, ya sea por show o trayendolo de un archivo 
---- Se le pasa el reverso de la funcion ->  map auxiliar3 $ lines a 
--crearOraculo::  [(Int,String)] -> [(Oraculo a)]
--crearOraculo ((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):[]) = [crearPrediccion pred1] 
--crearOraculo ((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred2):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg1):xs)= help xs [(crearPregunta preg1 (crearPrediccion pred2) (crearPrediccion pred1))]
--  where
--    help:: [(Int,String)] -> [(Oraculo a)] -> [(Oraculo a)]
--    help _ [] = []
--    help [] x = x
--    help ((_, 'P':'r':'e':'g':'u':'n':'t':'a':preg1):[]) lista  = (crearPregunta preg1  (head lista) (last lista)):[]
--    help ((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred2):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred3):xs) lista = help xs ((crearPrediccion pred3):(crearPrediccion pred2):(crearPrediccion pred1):lista)
--    help ((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred2):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg1):xs) lista = help xs ((crearPregunta preg1 (crearPrediccion pred2) (crearPrediccion pred1)):lista)
--    help ((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg1):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred2):xs) lista = help xs ((crearPrediccion pred2):(crearPregunta preg1 (crearPrediccion pred1) (lista!!0)):(tail lista))
--    help ((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg1):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg2):xs) lista = help xs ((crearPregunta preg2 (crearPregunta preg1 (crearPrediccion pred1) (lista!!0)) (lista !! 1)):(drop 2 lista))
--    help ((_,'P':'r':'e':'g':'u':'n':'t':'a':preg1):(_,'P':'r':'e':'g':'u':'n':'t':'a':preg2):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg3):xs)  lista = help xs ((crearPregunta preg3 (crearPregunta preg2 (crearPregunta preg1 (lista!!0) (lista!!1)) (lista!!2)) (lista!!3)):(drop 4 lista))
--    help ((_,'P':'r':'e':'g':'u':'n':'t':'a':preg2):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_, 'P':'r':'e':'g':'u':'n':'t':'a':preg1):xs)  lista = help xs ((crearPregunta preg1 (crearPrediccion pred1) (lista!!0)):(tail lista))
--    help ((_,'P':'r':'e':'g':'u':'n':'t':'a':preg1): e@((_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred1):(_,'P':'r':'e':'d':'i':'c':'c':'i':'o':'n':pred2):xs)) lista = help e ((crearPregunta preg1 (lista!!0) (lista !! 1)):(drop 2 lista))
    
    
    
----Detecta los hijos del nodo inicial de la lista, cuando consigue otro nodo a ese mismo nivel se sale
--detectarHijos :: Int -> [(Int,String)] -> Bool -> [(Int,String)]
--detectarHijos _ [] booleano = []
--detectarHijos pisoOrigen (e@(nivel,str):xs) booleano
--  | (pisoOrigen == nivel) && (not booleano) = [e]++(detectarHijos pisoOrigen xs True)
--  | pisoOrigen < nivel = [e]++(detectarHijos pisoOrigen xs True)
--  | (pisoOrigen == nivel) && (booleano) = []
  

---- Este auxiliar quita los primeros espacios que tenga un string
--auxiliar :: String -> String 
--auxiliar (x:xs)
--    | x == ' '  = auxiliar xs
--    | otherwise = x:xs

---- Este auxiliar cuenta cuantos espacios en blanco tiene un string al principio
--auxiliar2 :: String -> Int
--auxiliar2 (x:xs)
--    | x == ' '  = 1 + auxiliar2 xs
--    | otherwise = 0

---- te devuelve la tupla con la cantidad de espacios en blanco y la preg/prediccion
--auxiliar3 :: String -> (Int, String)
--auxiliar3 x = (auxiliar2 x, auxiliar x)

---- esta funcion es para determinar si una tupla es mayor o menor que otra
--mycompare :: Ord a => (a,b) -> (a,b) -> Ordering
--mycompare (a,_) (b,_)
--    | a <= b = LT
--    | a > b = GT

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
