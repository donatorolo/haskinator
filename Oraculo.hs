-- oraculo.hs
-- Autor:  Carlos Aponte 09-10041
--         Donato Rolo   10-10640

module Oraculo (Oraculo(Prediccion,Pregunta), crearPrediccion, crearPregunta, 
    prediccion, pregunta, positivo, negativo, obtenerCadena) where
--------------------------------------------------------------------------------
--                                LIBRERÍAS                                   --
--------------------------------------------------------------------------------

import Data.Maybe

--------------------------------------------------------------------------------
--                             ORACULOS PRUEBA                                --
--------------------------------------------------------------------------------



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
    deriving(Eq, Show, Ord)

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

--obtenerCadena ::  Oraculo a -> String -> Maybe [(String,Bool)]
--obtenerCadena a b = help [(a,[])] b
--        where
--            help :: [(Oraculo a,[(String,Bool)])] -> String -> Maybe [(String,Bool)]
--            help ((Prediccion a,c):xs) b
--                | a == b    = Just c
--                | otherwise = help xs b
--            help ((Pregunta a yes no,c):xs) b = help (xs ++ [(yes,c++[(a,True)]),(no,c++[(a,False)])]) b
--            help [] b = Nothing



            --help (Prediccion a) b c
            --    | a == b    = Just c
            --    | otherwise = Nothing
            --help (Pregunta a yes no) b c
            --    | help yes b (c++[(a,True)]) /= Nothing = help yes b (c++[(a,True)])
            --    | help no b (c++[(a,False)]) /= Nothing = help no b (c++[(a,False)])
            --    | otherwise = Nothing

obtenerCadena :: Oraculo a -> String -> Maybe [(String, Bool)]
obtenerCadena (Prediccion a) b 
    | a == b    = Just []
    | otherwise = Nothing
obtenerCadena (Pregunta a yes no) b = attach inYes $ if inYes then goYes else goNo
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
obtenerEstadisticas a b = (minimum l, maximum l,average l)
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
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


