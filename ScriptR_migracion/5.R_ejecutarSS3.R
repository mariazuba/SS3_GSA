
##############################################################################################
### Ejecutar SS3 luego de la Migración de a4a a SS3: Caso boqueron ----
##############################################################################################
library(here)
# CARGAR LIBRERIAS ----
source(here("ScriptR_migracion","Librerias_Requeridas.R")) 

# CARGAR DIRECTORIOS ----
source(here("ScriptR_migracion","Directorios_Requeridos.R")) 
#-----------------------------------------------------------------------------------------------
# Crea la carpeta Boqueron_SS3 ----
unlink(dir.boqueron, recursive = TRUE) #borra la carpeta
dir.create(file.path(dir.mod))#crea la carpeta  nuevamente para asegurar que los códigos 
                                   #que llamaremos más abajo funcionan bien

# llama los códigos de R para Genera los archivos requidos para que SS3 se ejecute ----
source(here("ScriptR_migracion","1.R_data.ss.R")) 
source(here("ScriptR_migracion","2.R_control.ss.R")) 
source(here("ScriptR_migracion","3.R_starter.ss.R")) 
source(here("ScriptR_migracion","4.R_forecast.ss.R")) 
#-----------------------------------------------------------------------------------------------

# Función que permite ejecutar el modelo ----
# depende de la versión descargada de r4ss, la última sólo utiliza r4ss::run()
r4ss::run_SS_models(dirvec=here(dir.mod), 
                    model=here(dir.exe,"ss_win.exe"),
                    skipfinished=FALSE)

# Al iniciar la ejecución, SS3 siempre lee los archivos 
# `starter.ss`, `data.ss` y `forecast.ss`, `control.ss` en el mismo orden, 
# escribiendo las salidas de debugging en `echoinput.sso` y 
# advertencias en `warning.sso` a medida que se leen.



