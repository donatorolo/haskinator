# Fecha: 02.02/2015
# 
# Autor:  Carlos Aponte 09-10041                                             --
#         Donato Rolo   10-10640   
# 
# Proyecto: Haskinator
#  
# Descripcion: Makefile para el proyecto Haskinator el cual incluye el Oraculo.hs y Haskinator.hs.
# HS_PROG -> Archivos a Compilar
# OUTPUT -> Nombre del Ejecutable resultante
# CC -> Compilador
# CFLAGS -> Flags de Compilacion
# DELCOM -> Comando para limpiar
# DELFILES -> Archivos a borrar luego de la produccion del ejecutable
# DELMAIN -> archivo ejecutable a borrar

CC = ghc
CFLAGS= -o
HS_PROG = Haskinator.hs
OUTPUT = Haskinator
DELCOM = rm 
DELFILES = *.o *.hi *~
DELMAIN = Haskinator
make:
	$(CC) $(CFLAGS) $(OUTPUT) $(HS_PROG)
	$(DELCOM) $(DELFILES)

.PHONY: clean
clean:
	$(DELCOM) $(DELMAIN)

	
	