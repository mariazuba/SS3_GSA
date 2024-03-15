# Generar archivos excell

library(here)
library(openxlsx)

# llama los códigos de R para Genera los archivos requidos para que SS3 se ejecute ----
#source(here("Migracion_Gadget-SS3_Caso_boqueron","ScriptR_migracion","1.R_data.ss.R")) 
source(here("ScriptR_migracion","2.R_control.ss.R")) 
source(here("ScriptR_migracion","3.R_starter.ss.R")) 
source(here("ScriptR_migracion","4.R_forecast.ss.R")) 

# Crear un archivo Excel
wb <- createWorkbook()


# capturas

#===============================================================================
# Datos de captura comercial ----
#===============================================================================
# Arreglo de Datos
year<-2003:2021
nyear<-length(year)
catch_year<-c(-999,year)
catch_seas<-rep(1,nyear+1)
catch_fleet<-rep(1,nyear+1)
catch_catch<-c(0,CATCH.DAT$X5) 
catch_catch_se<-rep(0.01,nyear+1) # se asume cv = 0.01 Revisar!!!!
#---------------------------------------------------------------------------------
# crear data.frame 
catch1<-data.frame(year=catch_year,
                   seas=catch_seas,
                   fleet=catch_fleet,
                   catch=catch_catch,
                   catch_se=catch_catch_se)

catch<-catch1
addWorksheet(wb, "catch")
writeData(wb, sheet = "catch", x = catch)

# índices de abundancia campañas
cpue <- dat1$CPUE
addWorksheet(wb, "survey")
writeData(wb, sheet = "survey", x = cpue)

# Composición de tallas
length <- dat1$lencomp
addWorksheet(wb, "lengthComp")
writeData(wb, sheet = "lengthComp", x = length)

# # Claves talla-edad
# agelength <- new_agecomp
# addWorksheet(wb, "agelength")
# writeData(wb, sheet = "agelength", x = agelength )


# Parámetros biológicos
parbio <- ctl1$MG_parms
parbio$par<-row.names(ctl1$MG_parms)
addWorksheet(wb, "MG_parms")
writeData(wb, sheet = "MG_parms", x = parbio )


# Relación stock-recluta
parSR <- ctl1$SR_parms
parSR$par<-row.names(ctl1$SR_parms)
addWorksheet(wb, "SR_parms")
writeData(wb, sheet = "SR_parms", x = parSR )

# Capturabilidad
parQ <- ctl1$Q_parms
parQ$par<-row.names(ctl1$Q_parms)
addWorksheet(wb, "Q_parms")
writeData(wb, sheet = "Q_parms", x = parQ )

# Selectividad por talla
parSel_size <- ctl1$size_selex_parms
parSel_size$par<-row.names(ctl1$size_selex_parms)
addWorksheet(wb, "size_selex_parms")
writeData(wb, sheet = "size_selex_parms", x = parSel_size )

# Selectividad por edad
parSel_age <- ctl1$age_selex_parms
parSel_age$par<-row.names(ctl1$age_selex_parms)
addWorksheet(wb, "age_selex_parms")
writeData(wb, sheet = "age_selex_parms", x = parSel_age )


# Guardar el archivo Excel
saveWorkbook(wb, paste(here("Archivos_datos",'Excell_SS3'),"datos_boqueron_SS3.xlsx",sep="/"), overwrite = TRUE)


