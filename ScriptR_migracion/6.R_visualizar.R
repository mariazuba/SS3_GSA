

##############################################################################################
### Migración de a4a a SS3: Caso boqueron - Visualizar Salidas ----
##############################################################################################
library(here)
# CARGAR LIBRERIAS ----
source(here("ScriptR_migracion","Librerias_Requeridas.R")) 

# CARGAR DIRECTORIOS ----
source(here("ScriptR_migracion","Directorios_Requeridos.R")) 


## Leer las salidas

# Leemos la salida del modelo SS3  con la función `SS_output()`

replist <- SS_output(dir=dir.mod,verbose=TRUE,printstats=TRUE)

SS_plots(replist)