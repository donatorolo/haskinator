################################################################################
# Fecha: 02/02/2015                                                            #
#                                                                              #
# Autor:  Carlos Aponte 09-10041                                               #
#         Donato Rolo   10-10640                                               #
#                                                                              #
# Proyecto: Haskinator                                                         #
#                                                                              #
# Descripcion: Makefile para el proyecto Haskinator el cual incluye el         #
#              Oraculo.hs y Haskinator.hs.                                     #
################################################################################


# CC -> Compilador
CC = ghc
HD = haddock
# CFLAGS -> Flags de Compilacion
CFLAGS= -o
HDFLAGS= -o . -h 
# HS_PROG -> Archivos a Compilar
HS_PROG = Haskinator.hs
# OUTPUT -> Nombre del Ejecutable resultante
OUTPUT = Haskinator
# DELCOM -> Comando para limpiar
DELCOM = rm 
# DELFILES -> Archivos a borrar luego de la produccion del ejecutable
DELFILES = *.o *.hi *~ 
# DELMAIN -> archivo ejecutable a borrar
DELMAIN = Haskinator *.html *.png *.jpg *.css *.gif *~  *.js

make:
	$(CC) $(CFLAGS) $(OUTPUT) $(HS_PROG)
	$(HD)  $(HDFLAGS) $(HS_PROG)
	$(DELCOM) $(DELFILES)

.PHONY: clean
clean:
	$(DELCOM) $(DELMAIN)