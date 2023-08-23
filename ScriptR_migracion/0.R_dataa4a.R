###############################################################################
### Leer datos Merluzas ----
###############################################################################

# Esta sección puede ser mejorada con las funciones de Rgadget(), tarea pendiente
library(here)
# CARGAR LIBRERIAS ----
source(here("ScriptR_migracion","Librerias_Requeridas.R")) 

# CARGAR DIRECTORIOS ----
source(here("ScriptR_migracion","Directorios_Requeridos.R")) 

# 1. LEER ARCHIVOS DE DATOS a4a ----

#### Capturas anuales del stock (toneladas)

CATCH.DAT <- read.table(paste(dir,"CATCH.DAT",sep="/"),
                        header=T,sep="",na="NA",fill=T,skip = 4)
CATCH.DAT

####  Matriz de número de individuos por edad/año del survey (Miles de individuos) 

TUNEFF.DAT <- read.table(paste(dir,"TUNEFF.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 6)
TUNEFF.DAT

TUNEFF <- TUNEFF.DAT[,2:7] %>%
  rowwise() %>%
  mutate(total = sum(c_across(everything()), na.rm = TRUE))

#### Matriz de número de individuos por edad/año de las capturas (Miles de individuos)

CATNUM.DAT <- read.table(paste(dir,"CATNUM.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 5)
CATNUM.DAT 

#### Peso medio por edad y año de tu matriz de captura (Kilos).

CATWT.DAT <- read.table(paste(dir,"CATWT.DAT",sep="/"),
                        sep="",na="NA",fill=T,skip = 5)
CATWT.DAT 

#### Medio por edad y año asumida para el stock (Normalmente = CATWT) (Kilos).

STOCWT.DAT <- read.table(paste(dir,"STOCWT.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 5)
STOCWT.DAT


#### Vector de mortalidad Natural por edad.

NATMOR.DAT <- read.table(paste(dir,"NATMOR.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 5)
NATMOR.DAT


#### Ogiva de madurez por edad.

PROPMAT.DAT <- read.table(paste(dir,"PROPMAT.DAT",sep="/"),
                          sep="",na="NA",fill=T,skip = 5)
PROPMAT.DAT

#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
# Gerena Rdata ----
#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
save(list=ls(all=T),
     file=paste(here("Archivos_datos",'Merluzas_a4a'),
                     "/Rdata_a4a.RData",sep=""))
#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
