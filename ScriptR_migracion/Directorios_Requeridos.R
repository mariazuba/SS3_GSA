###############################################################################
### Directorios requeridos para la Migración de a4a a SS3 ----
###############################################################################
library(here)
# Archivos_datos ----
## Merluzas_a4a: Archivos a4a con datos requeridos.
dir<-here("Archivos_datos",'Merluzas_a4a',"hke_GSA6_a4a_format")

## Ejemplos_SS3: Archivos base para modificar.
dir.base<-here("Archivos_datos",'Ejemplos_SS3','simple')

## Merluzas_SS3: Archivos modificados.
dir.mod<-here("Archivos_datos",'Merluzas_SS3')

# Ejecutables_SS3 ----
## Ejecutables para diferentes versiones de SS3 y sistemas operativos. 
# Para este ejercicio se trabaja con la versión *"3.30.18_release"*
dir.exe<-here('Ejecutables_SS3','3.30.18_release')

###############################################################################