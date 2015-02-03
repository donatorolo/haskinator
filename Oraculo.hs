--------------------------------------------------------------------------------
-- Oraculo.hs                                                                 --
--                                                                            --
-- Autor:  Carlos Aponte 09-10041                                             --
--         Donato Rolo   10-10640                                             --
--------------------------------------------------------------------------------

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
--                             TIPOS DE DATOS                                 --
--------------------------------------------------------------------------------

-- | Tipo Oraculo

-- |     Prediccion: 
--         Una cadena de caracteres con la predicción en cuestión.

-- |     Preguntas:
--         Una cadena de caracteres con la pregunta en cuestión
--         Un oráculo que corresponda a una respuesta positiva para la pregunta.
--         Un oráculo que corresponda a una respuesta negativa para la pregunta.
data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo
    deriving(Eq, Show, Read)
    
--instance Show (Oraculo) where
--    show a = help a 0
--        where
--            help (Prediccion a) n      = replicate (n*2) ' ' ++ 
--                                         "Prediccion " ++ "\"" ++ a ++ "\"" ++ 
--                                         "\n"
--            help (Pregunta a yes no) n = replicate (n*2) ' ' ++ "Pregunta " ++
--                                         "\"" ++ a ++ "\""  ++ "\n" ++ 
--                                         help yes (n+1) ++ help no (n+1)
                                         
--------------------------------------------------------------------------------
--                       FUNCIONES DE CONSTRUCCIÓN                            --
--------------------------------------------------------------------------------

-- | crearPrediccion: Esta función recibe una cadena de caracteres, y devuelve 
--                  un 'Oraculo' de tipo 'Prediccion'.
crearPrediccion :: String -> Oraculo
crearPrediccion = Prediccion

--------------------------------------------------------------------------------

-- | crearPregunta: Esta función recibe una cadena de caracteres y dos 
--                'Oraculos'. Y devuelve un 'Oraculo' de tipo Pregunta con la 
--                cadena suministrada como pregunta, el primer 'Oraculo' como 
--                respuesta positiva y el segundo como negativa.
crearPregunta:: String -> Oraculo -> Oraculo -> Oraculo
crearPregunta = Pregunta

--------------------------------------------------------------------------------
--                         FUNCIONES DE ACCESO                                --
--------------------------------------------------------------------------------

-- | prediccion: Esta función recibe un 'Oraculo' y devuelve la cadena de 
--             caracteres, siempre y cuando sea una 'Prediccion'.
prediccion :: Oraculo -> String
prediccion (Prediccion a) = a
prediccion _              = error 
                             "El elemento insertado no es del tipo 'Prediccion'"

--------------------------------------------------------------------------------

-- | pregunta:  Esta función recibe un 'Oraculo' y devuelve la cadena de 
--            caracteres, siempre y cuando sea una 'Pregunta'.
pregunta :: Oraculo -> String
pregunta (Pregunta a _ _) = a
pregunta _                = error 
                               "El elemento insertado no es del tipo 'Pregunta'"

--------------------------------------------------------------------------------

-- | positivo:  Esta función recibe un 'Oraculo' y devuelve el 'Oraculo' que 
--            corresponde a una respuesta postiva, siempre y cuando sea 
--            una 'Pregunta'.
positivo :: Oraculo -> Oraculo
positivo (Pregunta _ a _) = a
positivo _                = error 
                               "El elemento insertado no es del tipo 'Pregunta'"

--------------------------------------------------------------------------------

-- | negativo:  Esta función recibe un 'Oraculo' y devuelve el 'Oraculo' que 
--            corresponde a una respuesta negativa, siempre y cuando sea 
--            una 'Pregunta'.
negativo :: Oraculo -> Oraculo
negativo (Pregunta _ _ a) = a
negativo _                = error 
                               "El elemento insertado no es del tipo 'Pregunta'"

--------------------------------------------------------------------------------
--                       FUNCIONES DE MODIFICACIÓN                            --
--------------------------------------------------------------------------------

-- | obtenerCadena: Esta función recibe un 'Oraculo' y una cadena de caracteres 
--                correspondiente a una 'Prediccion'. Devuelve un valor de tipo 
--                'Maybe'. Si la 'Prediccion' suministrada no pertenece al 
--                'Oraculo', retorna 'Nothing'. De lo contrario retorna 
--                'Just lista', donde 'lista' es una lista de tuplas 
--                (String, Bool) que corresponden a todas las preguntas que 
--                deben hacerse a partir de la raíz del 'Oraculo', para alcanzar
--                la 'Prediccion' suministrada y el valor de verdad (decisión 
--                positiva o negativa) de la misma.
obtenerCadena :: Oraculo -> String -> Maybe [(String, Bool)]
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

-- | obtenerEstadisticas: Esta función recibe un 'Oraculo' y devuelve una 
--                     3-tupla con los siguientes datos: El mínimo, el máximo y 
--                     promedio de preguntas que el Oráculo necesita hacer para
--                     llegar a una predicción.
obtenerEstadisticas :: Oraculo -> String -> (Int,Int,Double)
obtenerEstadisticas a b  
  | l == [] = (0,0,0) 
  | otherwise = (minimum l, maximum l,average l)
    where
        l = getList a b [] 0
        getList :: Oraculo -> String -> [Int] -> Int -> [Int]
        getList (Prediccion a) b list counter
                | a == b    = counter:list
                |otherwise  = list
        getList (Pregunta a yes no) b list counter = list ++ goYes ++ goNo
            where
                goYes = getList yes b list (counter+1)
                goNo  = getList no b list (counter+1)
        average :: [Int] -> Double
        average l = (fromIntegral $ sum l) / (fromIntegral $ length l)

--------------------------------------------------------------------------------