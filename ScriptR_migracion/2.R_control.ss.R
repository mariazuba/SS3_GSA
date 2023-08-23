################################################################################
### Migración de a4a a SS3: Caso boqueron - Archivo control.ss ----
################################################################################
 library(here)

#===============================================================================
# CARGAR LIBRERIAS ----
#===============================================================================
source(here("ScriptR_migracion","Librerias_Requeridas.R")) 
#===============================================================================
# CARGAR DIRECTORIOS ----
#===============================================================================
source(here("ScriptR_migracion","Directorios_Requeridos.R")) 

# #===============================================================================
# # CARGAR RDATA: DATOS TRANSFORMADOS DE a4a EN FORMATO SS3 ----
# #===============================================================================
load(here("Archivos_datos",'Merluzas_a4a',"Rdata_a4a.RData"))


#===============================================================================
# Leer archivo control.ss ----
#===============================================================================
ctl <- r4ss::SS_readctl(
  file = here(dir.base,
              "control.ss"),
  version="3.3",
  verbose = FALSE,
  datlist = dat, use_datlist = TRUE)

#===============================================================================
# Modificar archivo control.ss  ----
#===============================================================================

# Lista para modificar
ctl1<-ctl 
# Contenido de la lista
names(ctl1) 

#===============================================================================
# Pesos medios a la edad y número de patrones de crecimiento y platoon ----
#===============================================================================
ctl1$EmpiricalWAA <- 0 # 0=Do not read the weight-at-age (wtatage.ss) file
ctl1$N_GP         <- 1 # 1=number (N) of growth (GP)
ctl1$N_platoon    <- 1 # 1=number of plattons within a growth pattern/morph
#===============================================================================
# Especificaciones de la Distribución del patrón reclutamiento ----
# por patrón de crecimiento, área y mes de reclutamiento
#===============================================================================
ctl1$recr_dist_method<-3 # 3 = option recruitment distribution method, 3=each settle entity
ctl1$recr_global_area<-1 # 1 = spawner-recruitment (not implement yet, but required), 1= global
ctl1$recr_dist_read  <-1 # 1 = number of recruitment settlement assignments
ctl1$recr_dist_inx   <-0 # 0 = future feature, not implement yet but required
#===============================================================================
# Distribución del patrón de reclutamiento ----
#===============================================================================
rec_pattern<-data.frame(row.names="recr_dist_pattern1",
                        "GPattern" = 1, # un patrón
                        "month"    = 1, # enero
                        "area"     = 1, # 1 área
                        "age"      = 0) # edad 0
ctl1$recr_dist_pattern<-rec_pattern

#===============================================================================
# Bloques ----
#===============================================================================
ctl1$N_Block_Designs    <- 1
ctl1$blocks_per_pattern <- 1
ctl1$Block_Design       <- c(1989,1989)

#===============================================================================
# Parámetros que varían en el tiempo ----
#===============================================================================
ctl1$time_vary_adjust_method <- 1
# arreglo de datos para "time_vary_auto_generation"
time_auto<-data.frame(matrix(rep(1,5),nrow=1,ncol=5))
colnames(time_auto)<-paste("time_vary_auto_generation_",seq(1,5,1),sep="")
ctl1$time_vary_auto_generation<-time_auto

#===============================================================================
# Especificaciones iniciales de parámetros biológicos ----
#===============================================================================

# Si natM_type = 3 (varía con la edad), utilizar el siguiente código
# ctl1$natM_type <- 3
# #------------------------------------------------------------------------------
# #incluir objeto a la lista para el vector de M a la edad
# medad0 <- list()
# medad1<-data.frame(matrix(c(2.21,1.3,1.3,1.3),nrow=1,ncol=4),row.names="#_natM1")
# colnames(medad1)<-paste("Age_",seq(0,3,1),sep="")
# medad0[[1]] <- medad1
# names(medad0)<-"natM"
# ctl1 <- append(ctl1, medad0, after = 28)


# Si natM_type = 0 (fijo para todas las edades), utilizar el siguiente código
ctl1$natM_type          <- 0 # 0=1 parámetro, fijo para todas las edades 
ctl1$GrowthModel        <- 1
ctl1$Growth_Age_for_L1  <- 0.1
ctl1$Growth_Age_for_L2  <- 4
ctl1$Exp_Decay          <- -999
ctl1$Growth_Placeholder <- 0
ctl1$N_natMparms        <- 1
ctl1$SD_add_to_LAA      <- 0
ctl1$CV_Growth_Pattern  <- 0
ctl1$maturity_option    <- 1
ctl1$First_Mature_Age   <- 1
ctl1$fecundity_option   <- 1
ctl1$hermaphroditism_option    <- 0
ctl1$parameter_offset_approach <- 1

#===============================================================================
# Valores de Parámetros biológicos ----
#===============================================================================

# Mortalidad natural
MG_parms1<-data.frame(#Parámetros
                      "LO"           = 0.05,
                      "HI"           = 2.5,
                      "INIT"         = 1.3,  # valor INIT y PRIOR tomado desde stockfile, se asume  fijo
                      "PRIOR"        = 1.3, # valor esperado ignorado si PR_type=0
                      #Error estándar
                      "PR_SD"        = 0.1, #este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = -4, # fijo (>0 se estima, <0 fijo)
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques (sin bloques)
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["NatM_p_1_Fem_GP_1",] <- MG_parms1 

# Crecimiento: Lmin
MG_parms2<-data.frame(#Parámetros
                      "LO"           = 0,
                      "HI"           = 15,
                      "INIT"         = 10.33, # valor tomado de WGHANSA report 2022 (Recruitment mean
                      "PRIOR"        = 32, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99, #este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 5, # se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["L_at_Amin_Fem_GP_1",]<-MG_parms2

# Crecimiento: Lmax
MG_parms3<-data.frame(#Parámetros
                      "LO"           = 5,
                      "HI"           = 22,
                      "INIT"         = 19, # tomado desde stockfile, fijo
                      "PRIOR"        = 19, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99, #este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 5, # se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["L_at_Amax_Fem_GP_1",]<-MG_parms3

# Crecimiento: K
MG_parms4<-data.frame(#Parámetros
                      "LO"           = 0.1,
                      "HI"           = 2.0,
                      "INIT"         = 0.89, # tomado desde stockfile, fijo
                      "PRIOR"        = 0.9, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99, #este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 3, # se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["VonBert_K_Fem_GP_1",]<-MG_parms4

# Crecimiento CV young
MG_parms5<-data.frame(#Parámetros
                      "LO"           = 0.03,
                      "HI"           = 0.150,
                      "INIT"         = 0.066,
                      "PRIOR"        = 0.1, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99, #este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 5, # se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["CV_young_Fem_GP_1",]<-MG_parms5

# Crecimiento CV old
MG_parms6<-data.frame(#Parámetros
                      "LO"           = 0.03,
                      "HI"           = 0.150,
                      "INIT"         = 0.066,
                      "PRIOR"        = 0.1, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 5, # se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["CV_old_Fem_GP_1",]<-MG_parms6

# Relación longitud-peso: a
MG_parms7<-data.frame(#Parámetros
                      "LO"           = -3.0,
                      "HI"           =  3.0,
                      "INIT"         = 0.00312895,# valor tomado de WGHANSA report 2022
                      "PRIOR"        = 0.00312895, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = -50, #fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["Wtlen_1_Fem_GP_1",]<-MG_parms7

# Relación longitud-peso: b
MG_parms8<-data.frame(#Parámetros
                      "LO"           = -3.0,
                      "HI"           =  4.0,
                      "INIT"         = 3.278,# valor tomado de WGHANSA report 2022
                      "PRIOR"        = 3.278, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = -50, # fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$MG_parms["Wtlen_2_Fem_GP_1",]<-MG_parms8

# Madurez: 50%
MG_parms9<-data.frame(#Parámetros
                       "LO"           = -3.0,
                       "HI"           =  15,
                       "INIT"         = 11.2,
                       "PRIOR"        = 0,
                       #Error
                       "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -50, #fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                      #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["Mat50%_Fem_GP_1",]<-MG_parms9

# Madurez: pendiente
MG_parms10<-data.frame(#Parámetros
                       "LO"           = -3.0,
                       "HI"           =  3.0,
                       "INIT"         = -0.45,
                       "PRIOR"        = -0.45, # valor esperado ignorado si PR_type=0
                       #Error
                       "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -50, #fijo
                       #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                       #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["Mat_slope_Fem_GP_1",]<-MG_parms10

# Fecundidad: intercepto
MG_parms11<-data.frame(#Parámetros
                       "LO"           = -3.0,
                       "HI"           =  3.0,
                       "INIT"         = 1,
                       "PRIOR"        = 1, # valor esperado ignorado si PR_type=0
                       #Error
                       "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -50, #fijo
                       #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                       #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["Eggs/kg_inter_Fem_GP_1",]<-MG_parms11

# Fecundidad: pendiente
MG_parms12<-data.frame(#Parámetros
                       "LO"           = -3.0,
                       "HI"           =  3.0,
                       "INIT"         = 1,
                       "PRIOR"        = 1, # valor esperado ignorado si PR_type=0
                       #Error
                       "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -50, #fijo
                       #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                       #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["Eggs/kg_slope_wt_Fem_GP_1",]<-MG_parms12

# Distribución del reclutamiento: area y mes
MG_parms13<-data.frame(#Parámetros
                       "LO"           = 0,
                       "HI"           = 10,
                       "INIT"         = 1,
                       "PRIOR"        = 1, # valor esperado ignorado si PR_type=0
                       #Error
                       "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -3, #fijo
                       #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                       #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["RecrDist_GP_1_area_1_month_1",]<-MG_parms13

# Crecimiento cohorte
MG_parms14<-data.frame(#Parámetros
                       "LO"           = 1,
                       "HI"           = 1,
                       "INIT"         = 1,
                       "PRIOR"        = 1, # valor esperado ignorado si PR_type=0
                       #Error
                       "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -1, #fijo
                       #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                       #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["CohortGrowDev",]<-MG_parms14

# Fracción de hembras
MG_parms15<-data.frame(#Parámetros
                       "LO"           = 0.000001, # el rango va entre 0 - 1 
                       "HI"           = 0.999999, # el rango va entre 0 - 1 
                       "INIT"         = 0.5, # se asume 50%hembras-50%machos
                       "PRIOR"        = 0.5, # valor esperado ignorado si PR_type=0
                       #Error
                       "PR_SD"        = 0.5, #este valor es ignorado si PR_type=0
                       "PR_type"      = 0, #0=no se usa (none)
                       #fase de estimación
                       "PHASE"        = -99, #fijo
                       #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                       "env_var&link" = 0,
                       "dev_link"     = 0,
                       "dev_minyr"    = 0,
                       "dev_maxyr"    = 0,
                       "dev_PH"       = 0,
                       #bloques
                       "Block"        = 0,
                       "Block_Fxn"    = 0)
ctl1$MG_parms["FracFemale_GP_1",]<-MG_parms15

#===============================================================================
# Código para eliminar objetos de la lista base que no utilizaremos ----
# Se eliminan los parámetros para machos. En el caso del boqueron no se utiliza 
# datos de machos, por lo tanto, los parámetros de machos se eliminan de la 
# lista de objetos del modelo base (simple).
#===============================================================================
patron_eliminar<-rownames(ctl1$MG_parms)[grep("_Mal_",rownames(ctl1$MG_parms))] 
ctl1$MG_parms <- subset(ctl1$MG_parms, !rownames(ctl1$MG_parms) %in%
                          c(patron_eliminar,"RecrDist_GP_1","RecrDist_Area_1","RecrDist_month_1"))

#===============================================================================
# Efecto estacional sobre parámetros biológicos ----
#===============================================================================
MGparm_seas_effects1<-data.frame(matrix(rep(0,10),nrow=1,ncol=10))
colnames(MGparm_seas_effects1)<-paste("MGparm_seas_effects_",seq(1,10,1),sep="")
ctl1$MGparm_seas_effects<-MGparm_seas_effects1

#===============================================================================
# Formato efecto estacional sobre parámetros biológicos ----
#===============================================================================
ctl1$MGparm_seas_effects

#===============================================================================
# Relación stock-recluta ----
#===============================================================================
ctl1$SR_function          <- 4 # 4=option SCAA (ignora el steepness)
ctl1$Use_steep_init_equi  <- 0
ctl1$Sigma_R_FofCurvature <- 0

#===============================================================================
# Parámetros de la relación stock-recluta ----
#===============================================================================
SR_parms1<-data.frame(#Parámetros
                      "LO"           = 5,
                      "HI"           = 20,
                      "INIT"         = 13,
                      "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 1, # se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$SR_parms[1,]<-SR_parms1
rownames(ctl1$SR_parms)[1]<-"SR_LN(R0)"

SR_parms2<-data.frame(#Parámetros
                      "LO"           = 0.2,
                      "HI"           = 1,
                      "INIT"         = 0.88,
                      "PRIOR"        = 0.777,
                      #Error
                      "PR_SD"        = 0.113,
                      "PR_type"      = 2, #2=full beta
                      #fase de estimación
                      "PHASE"        = -4, # fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$SR_parms[2,] <-SR_parms2; 
rownames(ctl1$SR_parms)[2]<-"SR_SCAA_null"

SR_parms3<-data.frame(#Parámetros
                      "LO"           = 0.3,
                      "HI"           = 1.6,
                      "INIT"         = 0.6,  # CV=0.6 usado en pelágicos chile
                      "PRIOR"        = 1.1, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99, #este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = -6, #fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$SR_parms[3,] <-SR_parms3
rownames(ctl1$SR_parms)[3]<-"SR_sigmaR"

SR_parms4<-data.frame(#Parámetros
                      "LO"           = -5,
                      "HI"           = 5,
                      "INIT"         = 0,  # sin cambio de régimen
                      "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = -50, # fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$SR_parms[4,] <-SR_parms4
rownames(ctl1$SR_parms)[4]<-"SR_regime"

# Autocorrelación en el reclutamiento
SR_parms5<-data.frame(#Parámetros
                      "LO"           = 0,
                      "HI"           = 2,
                      "INIT"         = 0, # sin autocorrelación en el reclutamiento
                      "PRIOR"        = 1, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 99,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = -50,#fijo
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$SR_parms[5,] <- SR_parms5
rownames(ctl1$SR_parms)[5]<-"SR_autocorr"


#===============================================================================
# Desvíos del reclutamiento ----
#===============================================================================

ctl1$do_recdev <- 1
ctl1$MainRdevYrFirst <- 1989  # primer año de la serie (timefile)
ctl1$MainRdevYrLast  <- 2022  # último año de la serie (timefile)
ctl1$recdev_phase    <- 1
ctl1$recdev_adv      <- 0
ctl1$recdev_early_start       <- NULL
ctl1$recdev_early_phase       <- NULL
ctl1$Fcast_recr_phase         <- NULL
ctl1$lambda4Fcast_recr_like   <- NULL
ctl1$last_early_yr_nobias_adj <- NULL
ctl1$first_yr_fullbias_adj    <- NULL
ctl1$last_yr_fullbias_adj     <- NULL
ctl1$first_recent_yr_nobias_adj <- NULL
ctl1$max_bias_adj               <- NULL
ctl1$period_of_cycles_in_recr <- NULL
ctl1$min_rec_dev    <- NULL
ctl1$max_rec_dev    <- NULL
ctl1$N_Read_recdevs <- NULL

#===============================================================================
# Mortalidad por pesca ----
#===============================================================================

ctl1$F_ballpark      <- ctl$F_ballpark # igual a base "simple"
ctl1$F_ballpark_year <- -1989 # valor negativo lo deshabilita
ctl1$F_Method        <- 3 #option 3=hibrida (recomendada SS3)
ctl1$maxF            <- ctl$maxF # valor máximo de F, en este caso se deja igual a base "simple"
ctl1$F_iter          <- ctl$F_iter # número de iteraciones dpara tuning de F en método híbrido, en este caso se deja igual a base "simple"

#===============================================================================
# Especificaciones para la Capturabilidad ----
#===============================================================================

Q_options1<-data.frame("fleet"     = 2, # 2 = pelago
                       "link"      = 1, # 1 = q simple, se asume proporcional: y=q*x
                       "link_info" = 0, # 0 = no hay información adicional
                       "extra_se"  = 0,  
                       "biasadj"   = 0,
                       "float"     = 0) 
ctl1$Q_options[1,]<-Q_options1
rownames(ctl1$Q_options)[1]<-"Pelago"

Q_options2<-data.frame("fleet"     = 3, # 3 = ecocadiz
                       "link"      = 1, # 1 = q simple, se asume proporcional: y=q*x
                       "link_info" = 0, # 0 = no hay información adicional
                       "extra_se"  = 0, 
                       "biasadj"   = 0,
                       "float"     = 0) 
ctl1$Q_options[2,]<-Q_options2
rownames(ctl1$Q_options)[2]<-"Ecocadiz"

#===============================================================================
# Parámetros de capturabilidad ----
#===============================================================================

Q_parms1<-data.frame(#Parámetros
                     "LO"           = -3,
                     "HI"           = 2,
                     "INIT"         = 0,
                     "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                     #Error
                     "PR_SD"        = 1,#este valor es ignorado si PR_type=0
                     "PR_type"      = 0, #0=no se usa (none)
                     #fase de estimación
                     "PHASE"        = 1,#se estima
                     #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                     "env_var&link" = 0,
                     "dev_link"     = 0,
                     "dev_minyr"    = 0,
                     "dev_maxyr"    = 0,
                     "dev_PH"       = 0,
                     #bloques
                     "Block"        = 0,
                     "Block_Fxn"    = 0)
ctl1$Q_parms[1,]<-Q_parms1
rownames(ctl1$Q_parms)[1]<-"LnQ_base_Pelago(2)"

Q_parms2<-data.frame( #Parámetros
                      "LO"           = -3,
                      "HI"           = 2,
                      "INIT"         = 0,
                      "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                      #Error
                      "PR_SD"        = 1,#este valor es ignorado si PR_type=0
                      "PR_type"      = 0, #0=no se usa (none)
                      #fase de estimación
                      "PHASE"        = 1, #se estima
                      #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                      "env_var&link" = 0,
                      "dev_link"     = 0,
                      "dev_minyr"    = 0,
                      "dev_maxyr"    = 0,
                      "dev_PH"       = 0,
                      #bloques
                      "Block"        = 0,
                      "Block_Fxn"    = 0)
ctl1$Q_parms[2,]<-Q_parms2
rownames(ctl1$Q_parms)[2]<-"LnQ_base_Ecocadiz(3)"

#===============================================================================
# Código para eliminar objetos de la lista base que no utilizaremos ----
#===============================================================================
ctl1$Q_parms <- subset(ctl1$Q_parms, !rownames(ctl1$Q_parms) %in% c("LnQ_base_SURVEY2(3)"))

#===============================================================================
# Tipos de patrones de selectividad a la talla ----
#===============================================================================

size_selex_types1<-data.frame("Pattern" = 1, # se asume la misma selectividad para todas las tallas
                              "Discard" = 0, # 0=sin información
                              "Male"    = 0, # 0=sin información
                              "Special" = 0) # 0=sin información
ctl1$size_selex_types[1,]<-size_selex_types1
rownames(ctl1$size_selex_types)[1]<-"Flota"

size_selex_types2<-data.frame("Pattern" = 1,  # se asume la misma selectividad para todas las tallas
                              "Discard" = 0, # 0=sin información
                              "Male"    = 0, # 0=sin información
                              "Special" = 0) # 0=sin información
ctl1$size_selex_types[2,]<-size_selex_types2
rownames(ctl1$size_selex_types)[2]<-"Pelago"

size_selex_types3<-data.frame("Pattern" = 1,  # se asume la misma selectividad para todas las tallas
                              "Discard" = 0, # 0=sin información
                              "Male"    = 0, # 0=sin información
                              "Special" = 0) # 0=sin información
ctl1$size_selex_types[3,]<-size_selex_types3
rownames(ctl1$size_selex_types)[3]<-"Ecocadiz"

#===============================================================================
# Tipos de patrones de selectividad a la edad ----
#===============================================================================

age_selex_types1<-data.frame(Pattern = 12, # se asume logística
                             Discard = 0,  # 0=sin información
                             Male    = 0,  # 0=sin información
                             Special = 0)  # 0=sin información
ctl1$age_selex_types[1,]<-age_selex_types1
rownames(ctl1$age_selex_types)[1]<-"Flota"

age_selex_types2<-data.frame(Pattern = 12, # se asume logística
                             Discard = 0,  # 0=sin información
                             Male    = 0,  # 0=sin información
                             Special = 0)  # 0=sin información
ctl1$age_selex_types[2,]<-age_selex_types2
rownames(ctl1$age_selex_types)[2]<-"Pelago"

age_selex_types3<-data.frame(Pattern = 12, # se asume logística
                             Discard = 0,  # 0=sin información
                             Male    = 0,  # 0=sin información
                             Special = 0)  # 0=sin información
ctl1$age_selex_types[3,]<-age_selex_types3
rownames(ctl1$age_selex_types)[3]<-"Ecocadiz"

#===============================================================================
# Parámetros de selectividad a la talla ----
#===============================================================================

size_selex_parms1<-data.frame(#Parámetros
                              "LO"           = -1,
                              "HI"           = 20,
                              "INIT"         = 12.6, # valor tomado de WGHANSA report 2022
                              "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                              #Error
                              "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                              "PR_type"      = 0, #0=no se usa (none)
                              #fase de estimación
                              "PHASE"        = 2,#se estima
                              #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                              "env_var&link" = 0,
                              "dev_link"     = 0,
                              "dev_minyr"    = 0,
                              "dev_maxyr"    = 0,
                              "dev_PH"       = 0.5,
                              #bloques
                              "Block"        = 0,
                              "Block_Fxn"    = 0)
ctl1$size_selex_parms[1,]<-size_selex_parms1
rownames(ctl1$size_selex_parms)[1]<-"SizeSel_P_1_Flota(1)"

size_selex_parms2<-data.frame(#Parámetros
                              "LO"           = -1,
                              "HI"           = 3,
                              "INIT"         = 0.193, # valor tomado de WGHANSA report 2022
                              "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                              #Error
                              "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                              "PR_type"      = 0, #0=no se usa (none)
                              #fase de estimación
                              "PHASE"        = 2,#se estima
                              #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                              "env_var&link" = 0,
                              "dev_link"     = 0,
                              "dev_minyr"    = 0,
                              "dev_maxyr"    = 0,
                              "dev_PH"       = 0.5,
                              #bloques
                              "Block"        = 0,
                              "Block_Fxn"    = 0)
ctl1$size_selex_parms[2,]<-size_selex_parms2
rownames(ctl1$size_selex_parms)[2]<-"SizeSel_P_2_Flota(1)"

size_selex_parms3<-data.frame(#Parámetros
                              "LO"           = -3,
                              "HI"           = 20,
                              "INIT"         = 14.3, # valor tomado de WGHANSA report 2022
                              "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                              #Error
                              "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                              "PR_type"      = 0, #0=no se usa (none)
                              #fase de estimación
                              "PHASE"        = 3,#se estima
                              #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                              "env_var&link" = 0,
                              "dev_link"     = 0,
                              "dev_minyr"    = 0,
                              "dev_maxyr"    = 0,
                              "dev_PH"       = 0.5,
                              #bloques
                              "Block"        = 0,
                              "Block_Fxn"    = 0)
ctl1$size_selex_parms[3,]<-size_selex_parms3
rownames(ctl1$size_selex_parms)[3]<-"SizeSel_P_1_Pelago(2)"

size_selex_parms4<-data.frame(#Parámetros
                              "LO"           = -3,
                              "HI"           = 3,
                              "INIT"         = 0.406, # valor tomado de WGHANSA report 2022
                              "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                              #Error
                              "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                              "PR_type"      = 0, #0=no se usa (none)
                              #fase de estimación
                              "PHASE"        = 3,#se estima
                              #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                              "env_var&link" = 0,
                              "dev_link"     = 0,
                              "dev_minyr"    = 0,
                              "dev_maxyr"    = 0,
                              "dev_PH"       = 0.5,
                              #bloques
                              "Block"        = 0,
                              "Block_Fxn"    = 0)
ctl1$size_selex_parms[4,]<-size_selex_parms4
rownames(ctl1$size_selex_parms)[4]<-"SizeSel_P_2_Pelago(2)"

size_selex_parms5<-data.frame(#Parámetros
                              "LO"           = -1,
                              "HI"           = 20,
                              "INIT"         = 10.8,  # valor tomado de WGHANSA report 2022
                              "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                              #Error
                              "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                              "PR_type"      = 0, #0=no se usa (none)
                              #fase de estimación
                              "PHASE"        = 3,#se estima
                              #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                              "env_var&link" = 0,
                              "dev_link"     = 0,
                              "dev_minyr"    = 0,
                              "dev_maxyr"    = 0,
                              "dev_PH"       = 0.5,
                              #bloques
                              "Block"        = 0,
                              "Block_Fxn"    = 0)
ctl1$size_selex_parms[5,]<-size_selex_parms5
rownames(ctl1$size_selex_parms)[5]<-"SizeSel_P_1_Ecocadiz(3)"

size_selex_parms6<-data.frame(#Parámetros
                              "LO"           = -1,
                              "HI"           = 5,
                              "INIT"         = 0.764, # valor tomado de WGHANSA report 2022
                              "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
                              #Error
                              "PR_SD"        = 0,#este valor es ignorado si PR_type=0
                              "PR_type"      = 0, #0=no se usa (none)
                              #fase de estimación
                              "PHASE"        = 3,#se estima
                              #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                              "env_var&link" = 0,
                              "dev_link"     = 0,
                              "dev_minyr"    = 0,
                              "dev_maxyr"    = 0,
                              "dev_PH"       = 0.5,
                              #bloques
                              "Block"        = 0,
                              "Block_Fxn"    = 0)
ctl1$size_selex_parms[6,]<-size_selex_parms6
rownames(ctl1$size_selex_parms)[6]<-"SizeSel_P_2_Ecocadiz(3)"

#===============================================================================
# Parámetros de selectividad a la edad ----
#===============================================================================

age_selex_parms1<-data.frame(#Parámetros
                             "LO"            = -2.0,
                             "HI"            = 100,
                             "INIT"          = 100, # valor tomado de WGHANSA report 2022
                             "PRIOR"         = 0, # valor esperado ignorado si PR_type=0
                             #Error
                             "PR_SD"         = 0.01,#este valor es ignorado si PR_type=0
                             "PR_type"       = 0, #0=no se usa (none)
                             #fase de estimación
                             "PHASE"         = -1,#fijo
                             #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                             "env_var&link"  = 0,
                             "dev_link"      = 0,
                             "dev_minyr"     = 0,
                             "dev_maxyr"     = 0,
                             "dev_PH"        = 0.5,
                             #bloques
                             "Block"         = 0,
                             "Block_Fxn"     = 0)
ctl1$age_selex_parms[1,]<-age_selex_parms1
rownames(ctl1$age_selex_parms)[1]<-"AgeSel_P_1_Flota(1)"

age_selex_parms2<-data.frame(#Parámetros
                             "LO"            = -100,
                             "HI"            = 100,
                             "INIT"          = -100,
                             "PRIOR"         = 0, # valor esperado ignorado si PR_type=0
                             #Error
                             "PR_SD"         = 0.01,#este valor es ignorado si PR_type=0
                             "PR_type"       = 0, #0=no se usa (none)
                             #fase de estimación
                             "PHASE"         = -1,#fijo
                             #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                             "env_var&link"  = 0,
                             "dev_link"      = 0,
                             "dev_minyr"     = 0,
                             "dev_maxyr"     = 0,
                             "dev_PH"        = 0.5,
                             #bloques
                             "Block"         = 0,
                             "Block_Fxn"     = 0)
ctl1$age_selex_parms[2,]<-age_selex_parms2
rownames(ctl1$age_selex_parms)[2]<-"AgeSel_P_2_Flota(1)"

age_selex_parms3<-data.frame(#Parámetros
                             "LO"            = -2.0,
                             "HI"            = 100,
                             "INIT"          = 100,
                             "PRIOR"         = 0, # valor esperado ignorado si PR_type=0
                             #Error
                             "PR_SD"         = 0.01,#este valor es ignorado si PR_type=0
                             "PR_type"       = 0, #0=no se usa (none)
                             #fase de estimación
                             "PHASE"         = -1,#fijo
                             #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                             "env_var&link"  = 0,
                             "dev_link"      = 0,
                             "dev_minyr"     = 0,
                             "dev_maxyr"     = 0,
                             "dev_PH"        = 0.5,
                             #bloques
                             "Block"         = 0,
                             "Block_Fxn"     = 0)
ctl1$age_selex_parms[3,]<-age_selex_parms3
rownames(ctl1$age_selex_parms)[3]<-"AgeSel_P_1_Pelago(2)"

age_selex_parms4<-data.frame(#Parámetros
                             "LO"            = -100,
                             "HI"            = 100,
                             "INIT"          = -100,
                             "PRIOR"         = 0, # valor esperado ignorado si PR_type=0
                             #Error
                             "PR_SD"         = 0.01,#este valor es ignorado si PR_type=0
                             "PR_type"       = 0, #0=no se usa (none)
                             #fase de estimación
                             "PHASE"         = -1,#fijo
                             #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                             "env_var&link"  = 0,
                             "dev_link"      = 0,
                             "dev_minyr"     = 0,
                             "dev_maxyr"     = 0,
                             "dev_PH"        = 0.5,
                             #bloques
                             "Block"         = 0,
                             "Block_Fxn"     = 0)
ctl1$age_selex_parms[4,]<-age_selex_parms4
rownames(ctl1$age_selex_parms)[4]<-"AgeSel_P_2_Pelago(2)"

age_selex_parms5<-data.frame(#Parámetros
                             "LO"            = -2.0,
                             "HI"            = 100,
                             "INIT"          = 100,
                             "PRIOR"         = 0, # valor esperado ignorado si PR_type=0
                             #Error
                             "PR_SD"         = 0.01,#este valor es ignorado si PR_type=0
                             "PR_type"       = 0, #0=no se usa (none)
                             #fase de estimación
                             "PHASE"         = -1,#fijo
                             #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                             "env_var&link"  = 0,
                             "dev_link"      = 0,
                             "dev_minyr"     = 0,
                             "dev_maxyr"     = 0,
                             "dev_PH"        = 0.5,
                             #bloques
                             "Block"         = 0,
                             "Block_Fxn"     = 0)
ctl1$age_selex_parms[5,]<-age_selex_parms5
rownames(ctl1$age_selex_parms)[5]<-"AgeSel_P_1_Ecocadiz(3)"

age_selex_parms6<-data.frame(#Parámetros
                             "LO"            = -100,
                             "HI"            = 100,
                             "INIT"          = -100,
                             "PRIOR"         = 0, # valor esperado ignorado si PR_type=0
                             #Error
                             "PR_SD"         = 0.01,#este valor es ignorado si PR_type=0
                             "PR_type"       = 0, #0=no se usa (none)
                             #fase de estimación
                             "PHASE"         = -1, #fijo
                             #variabilidad en el tiempo  (Se asumen invariantes para boqueron )
                             "env_var&link"  = 0,
                             "dev_link"      = 0,
                             "dev_minyr"     = 0,
                             "dev_maxyr"     = 0,
                             "dev_PH"        = 0.5,
                             #bloques
                             "Block"         = 0,
                             "Block_Fxn"     = 0)
ctl1$age_selex_parms[6,]<-age_selex_parms6
rownames(ctl1$age_selex_parms)[6]<-"AgeSel_P_2_Ecocadiz(3)"

#===============================================================================
# Otros parámetros ----
#===============================================================================

ctl1$Use_2D_AR1_selectivity <-0
ctl1$TG_custom              <-0
ctl1$DoVar_adjust           <-1

varadj0 <- list()
varadj1<-data.frame(row.names=c("Variance_adjustment_list1",
                                "Variance_adjustment_list2",
                                "Variance_adjustment_list3"),
                    "Factor"  = c(4,4,4),
                    "Fleet"   = c(1,2,3),
                    "Value"   = c(0.0045,0.0051,0.0047))
varadj0[[1]] <- varadj1
names(varadj0)<-"Variance_adjustment_list"

ctl1 <- append(ctl1, varadj0, after = 66)

ctl1$Variance_adjustment_list

ctl1$maxlambdaphase          <- 1
ctl1$sd_offset               <- 1
ctl1$lambdas                 <- NULL
ctl1$N_lambdas               <- 0
ctl1$more_stddev_reporting   <- 0
ctl1$stddev_reporting_specs  <- NULL
ctl1$stddev_reporting_selex  <- NULL
ctl1$stddev_reporting_growth <- NULL
ctl1$stddev_reporting_N_at_A <- NULL

#===============================================================================
# Escribir archivo modificado `control.ss` ----
#===============================================================================

r4ss::SS_writectl(ctl1,
                  outfile=here(dir.mod,"control_GSA6.ss"),
                  overwrite = TRUE)

#===============================================================================