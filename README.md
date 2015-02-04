# haskinator

--------------------------------------------------------------------------------
-- Proyecto: Haskinator                                                       --
--                                                                            --
-- Fecha: 02/02/2015                                                          --
--                                                                            --
-- Autor:                                                                     --
--         Carlos Aponte 09-10041                                             --
--         Donato Rolo   10-10640                                             --
--------------------------------------------------------------------------------

------------------------------------
    Introducción:
------------------------------------

    Con este archivo Readme se busca aclarar posible duda sobre el contenido de 
los archivos del proyecto Haskinator, así como de las funciones que se 
implementaron, su accionar, y como se interrelacionan entre sí.

------------------------------------
    Documentación:
------------------------------------

    Para generar la documentacion del proyecto basta con ejecutar el comando 
haddock:

        haddock -o . -h Haskinator.hs   

------------------------------------
    Módulos: 
------------------------------------

Oraculo.hs -> contiene la definición del tipo Oraculo, el cual esta formado por 
          una 'Prediccion' o una pregunta, la cual puede tener como hijos 
          'Preguntas' o 'Predicciones'. Así mismo, contiene los métodos básicos
          para crear, modificar acceder y consultar un 'Oraculo'. De este módulo
          sólo son exportadas sus funciones principales las cuales son:
          
          - crearPrediccion: Esta función recibe una cadena de caracteres, y 
                devuelve un 'Oraculo' de tipo 'Prediccion'.
        
          - crearPregunta: Esta función recibe una cadena de caracteres y dos 
                'Oraculos'. Y devuelve un 'Oraculo' de tipo 'Pregunta' con la 
                cadena suministrada como pregunta, el primer 'Oraculo' como 
                respuesta positiva y el segundo como negativa.
        
          - prediccion: Esta función recibe un 'Oraculo' y devuelve la cadena de
                caracteres, siempre y cuando sea una 'Prediccion'.

          - pregunta: Esta función recibe un 'Oraculo' y devuelve la cadena de 
                caracteres, siempre y cuando sea una 'Pregunta'.

          - positivo: Esta función recibe un 'Oraculo' y devuelve el 'Oraculo' 
                que corresponde a una respuesta postiva, siempre y cuando sea 
                una 'Pregunta'.

          - negativo:  Esta función recibe un 'Oraculo' y devuelve el 'Oraculo' 
                que corresponde a una respuesta negativa, siempre y cuando sea 
                una 'Pregunta'.

          - obtenerCadena: Esta función recibe un 'Oraculo' y una cadena de 
                caracteres correspondiente a una 'Prediccion'. Devuelve un valor
                de tipo  'Maybe'. Si la 'Prediccion' suministrada no pertenece 
                al 'Oraculo', retorna 'Nothing'. De lo contrario retorna 
                'Just lista', donde 'lista' es una lista de tuplas 
                (String, Bool) que corresponden a todas las preguntas que deben 
                hacerse a partir de la raíz del 'Oraculo', para alcanzar la 
                'Prediccion' suministrada y el valor de verdad (es decir, 
                positiva o negativa) de la misma.

         - obtenerEstadisticas: Esta función recibe un 'Oraculo' y devuelve una 
                3-tupla con los siguientes datos: El mínimo, el máximo y 
                promedio de preguntas que el Oráculo necesita hacer para llegar
                a una predicción.
        
        
Haskinator.hs -> contiene la definición de un cliente del 'Oraculo' llamado 
        Haskinator, el cual al ser compilado genera un ejecutable el cual le 
        brinda al usuario un meno para interactuar con el 'Oraculo' 
        (reiniciarlo, actualizarlo, guardarlo, cargarlo, etc). Este archivo solo
        exporta el metodo Main el cual inicia el menu antes mencionado; sin 
        embargo, las funciones principales que encontramos en este archivo son:
        
          -Estadisticas: esta funcion recibe como parametro un 'Maybe Oraculo' y
                le pide al usuario una cadena de caracteres correspondiente a 
                una 'Prediccion'. Devuelve como resultado las estadisticas de
                minimo, maximo y promedio de preguntas necesarias para llegar a 
                esa 'Prediccion'; en caso de que la 'Prediccion' no este 
                contenida en el 'Oraculo', lo notifica y pide una nueva 
                'Prediccion'.

------------------------------------   
      Compilación y Uso
------------------------------------ 
     
     Compilar -> Para compilar el programa solo es necesario usar el comando 
            make, el cual procederá a hacer la compilación, eliminar los 
            archivos innecesarios y a producir un archivo ejecutable con el 
            nombre Haskinator. 
     
            Si se desea eliminar el Haskinator se puede ejecutar el comendo 
            "make clean" el cual sólo dejará los arhivos fuente, eliminando todo
            lo anteriormente generado por el make.
     
     Uso ->  Para usar el Haskinator basta con ejecutar el archivo antes 
            mencionado y seguir las instrucciones del menú de acuerdo a nuestro
            interés.
         
------------------------------------   
        Contacto
------------------------------------ 

09-10041@ldc.usb.ve
donatorolo93@gmail.com
