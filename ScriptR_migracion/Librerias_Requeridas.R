###############################################################################
### Librerías requeridas para la migración de Gadget2 a SS3: Caso boqueron ----

paquetes <- c("stringr", "tidyverse", "kableExtra","ggplot2","ggthemes",
              "patchwork","dplyr","reshape","here","r4ss","zoo")
lapply(paquetes, require, character.only = TRUE)

###############################################################################