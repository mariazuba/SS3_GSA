###############################################################################
### Leer datos Merluzas ----
###############################################################################

# Esta sección puede ser mejorada con las funciones de Rgadget(), tarea pendiente


library(readxl)
library(openxlsx)
# CARGAR LIBRERIAS ----
paquetes <- c("stringr", "tidyverse", "kableExtra","ggplot2","ggthemes",
              "patchwork","dplyr","reshape","here","r4ss","zoo")
lapply(paquetes, require, character.only = TRUE)

# CARGAR DIRECTORIOS ----
# Archivos_datos ----
## Merluzas_a4a: Archivos a4a con datos requeridos.
dir<-here("Archivos_datos",'Merluzas_a4a',"hke-GSA1-a4a_ format" )

tallasexcell<-here("Archivos_datos","Merluzas_a4a","Indice_abundancia_tallas_medits_GSA1_hake.xlsx")

archivoExcell<-"datos_GSA1_SS3.xlsx"

dir.excell<-paste(here("Archivos_datos",'Excell_SS3'),archivoExcell,sep="/")
# 1. LEER ARCHIVOS DE DATOS a4a ----

#### Capturas anuales del stock (toneladas)

CATCH.DAT <- read.table(paste(dir,"CATCH.DAT",sep="/"),
                        header=T,sep="",na="NA",fill=T,skip = 4)

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
catch1<-data.frame(Yr=catch_year,
                   seas=catch_seas,
                   fleet=catch_fleet,
                   catch=catch_catch,
                   catch_se=catch_catch_se)

# INDICE DE ABUNDANCIA
# Paso 1: leer archivo TUNEFF.DAT,  Matriz de número de individuos por edad/año del survey (Miles de individuos) 
TUNEFF.DAT <- read.table(paste(dir,"TUNEFF.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 6)
# Paso 2: calcular abundancia total del survey
# Índice de abundancia del survey formato a4a ----
SurveyIndex <- TUNEFF.DAT[,2:7] %>%
  rowwise() %>%
  mutate(total = sum(c_across(everything()), na.rm = TRUE))

# Paso 3: formato SS3 para el índice de abundancia survey
# Índice de abundancia del survey para formato SS3 ----

yearf1<-2003:2021
nyearf1<-length(yearf1)

CPUE_year<-c(yearf1)
CPUE_seas  <-rep(1,nyearf1) 
CPUE_index <-c(rep(2,nyearf1)) 
CPUE_obs   <-c(SurveyIndex$total) # migra desde a4a
CPUE_se_log<-c(rep(0.3,nyearf1)) 

# Genera data.frame
CPUE1<-data.frame(Yr   = CPUE_year, #  años
                  seas   = CPUE_seas, # temporada/mes
                  index  = CPUE_index, # número de cada flota 
                  obs    = CPUE_obs,    # valor observado
                  se_log = CPUE_se_log) # coeficiente de variación asumido en 0.3

# Paso 4: identificar matriz se composición de edad del survey
# Matriz de composición de edad del survey
AgeComp_survey<-TUNEFF.DAT[,2:7]
AgeComp_survey[is.na(AgeComp_survey)] <- 0
# Paso 5: formato SS3 para la composición de edad del survey
yearf2<-2003:2021
nyearf2<-length(yearf2)

new_agecomp_survey<-data.frame(Yr=yearf2, 
                        Seas=rep(7,nyearf2), 
                        FltSvy=rep(1,nyearf2),
                        Gender=rep(3,nyearf2), 
                        Part=rep(0,nyearf2),  
                        Ageerr=rep(2,nyearf2), 
                        Lbin_lo=rep(-1,nyearf2), 
                        Lbin_hi=rep(-1,nyearf2), 
                        Nsamp=rep(75,nyearf2))

age<-seq(0,5,1)
dat_rows_names<-paste("E",age,sep="")
names(AgeComp_survey)<-dat_rows_names 
new_agecomp_survey<-cbind(new_agecomp_survey, AgeComp_survey)


#### Matriz de número de individuos por edad/año de las capturas (Miles de individuos)

CATNUM.DAT <- read.table(paste(dir,"CATNUM.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 5)

CATNUM.DAT[is.na(CATNUM.DAT)] <- 0


yearf3<-2003:2021
nyearf3<-length(yearf3)

new_agecomp_catch<-data.frame(Yr=yearf3, 
                        Seas=rep(7,nyearf3), 
                        FltSvy=rep(1,nyearf3),
                        Gender=rep(3,nyearf3), 
                        Part=rep(0,nyearf3),  
                        Ageerr=rep(2,nyearf3), 
                        Lbin_lo=rep(-1,nyearf3), 
                        Lbin_hi=rep(-1,nyearf3), 
                        Nsamp=rep(75,nyearf3))

names(CATNUM.DAT)<-dat_rows_names 

new_agecomp_catch<-cbind(new_agecomp_catch, CATNUM.DAT)


#### Peso medio por edad y año de tu matriz de captura (Kilos).

CATWT.DAT <- read.table(paste(dir,"CATWT.DAT",sep="/"),
                        sep="",na="NA",fill=T,skip = 5)

#### Medio por edad y año asumida para el stock (Normalmente = CATWT) (Kilos).

STOCWT.DAT <- read.table(paste(dir,"STOCWT.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 5)

#### Vector de mortalidad Natural por edad.

NATMOR.DAT <- read.table(paste(dir,"NATMOR.DAT",sep="/"),
                         sep="",na="NA",fill=T,skip = 5)
#### Ogiva de madurez por edad.

PROPMAT.DAT <- read.table(paste(dir,"PROPMAT.DAT",sep="/"),
                          sep="",na="NA",fill=T,skip = 5)

# Datos de madurez , pesos medios a la edad 
years4<-2003:2021
nyears4<-length(years4)

maturity1<-as.numeric(PROPMAT.DAT[1,])
data_wt<-as.data.frame(CATWT.DAT) 

wt_flt<-data.frame(do.call(rbind,replicate(2,data_wt,simplify = FALSE))) %>% 
  mutate(type=c(rep("#wt_flt_1",nyears4),rep("#wt_flt_2",nyears4)))
colnames(wt_flt)<-c("E0","E1","E2","E3","E4","E5","#type")

fecundity<-data.frame(do.call(rbind,replicate(nyears4,maturity1,simplify = FALSE)))%>% 
  mutate(type=c(rep("#fecundity",nyears4))) 
colnames(fecundity)<-c("E0","E1","E2","E3","E4","E5","#type")

popwt<-data.frame(do.call(rbind,replicate(2,data_wt,simplify = FALSE))) %>% 
  mutate(type=c(rep("#popwt_beg",nyears4),rep("#popwt_mid",nyears4)))
colnames(popwt)<-c("E0","E1","E2","E3","E4","E5","#type")


wtatege_f1<-data.frame(Yr=c(rep(years4,5)), 
                       Seas=1, 
                       Sex=1, 
                       Bio_Pattern=1,
                       BirthSeas=1,
                       Fleet=c(rep(1,nyears4),rep(2,nyears4),
                               rep(-2,nyears4),
                               rep(0,nyears4),rep(-1,nyears4))) 

wtatege_f1<-cbind(wtatege_f1,rbind(wt_flt,fecundity,popwt))


wtat1<-wtatege_f1[order(wtatege_f1$Yr),]

# Composiciones de tallas


comptallas<-read_excel(tallasexcell, sheet = "catch",
                       col_names= TRUE,col_types=NULL,na="",skip= 0)

yearcatch<-seq(2003,2021,1)
nyearcatch<-length(yearcatch)
tallas<-seq(4,75,1)
comptallas<-comptallas[2:73,2:20]

CompTallas<-data.frame(row.names = NULL,year=yearcatch,t(comptallas))
colnames(CompTallas)<-c("year",tallas)

new_lencomp <- data.frame(Yr     = yearcatch, 
                          Seas   = rep(1,nyearcatch), 
                          FltSvy = rep(1,nyearcatch), 
                          Gender = rep(3,nyearcatch), 
                          Part   = rep(0,nyearcatch), 
                          Nsamp  = rep(100,nyearcatch))

dat_rows_names <- paste("L",tallas,sep="")

dat_rows <- CompTallas[,2:73]
names(dat_rows)<-dat_rows_names 
new_lencomp1<-cbind(new_lencomp, dat_rows)

#------------------------------------------------------------------------

Indtallas0<-read_excel(tallasexcell, sheet = "indices",
                      col_names= TRUE,col_types=NULL,na="",skip= 1)

yearcatch2<-seq(2003,2021,1)
nyearcatch2<-length(yearcatch2)
tallas2<-seq(5,65,1)
Indtallas<-Indtallas0[2:62,10:28]

IndicesTallas<-data.frame(row.names = NULL,year=yearcatch2,t(Indtallas))

colnames(IndicesTallas)<-c("year",tallas2)

new_lencomp2 <- data.frame(Yr     = yearcatch2, 
                          Seas   = rep(1,nyearcatch2), 
                          FltSvy = rep(2,nyearcatch2), 
                          Gender = rep(3,nyearcatch2), 
                          Part   = rep(0,nyearcatch2), 
                          Nsamp  = rep(100,nyearcatch2))

dat_rows_names2 <- paste("L",tallas2,sep="")

dat_rows2 <- IndicesTallas[,2:62]
names(dat_rows2)<-dat_rows_names2 
new_lencomp2<-cbind(new_lencomp2, dat_rows2)

####################################################
# PARAMETROS BIOLÓGICOS
####################################################

MG_parms1<-data.frame("LO"           = 0.05,
                      "HI"           = 0.4,
                      "INIT"         = 0.18,
                      "PRIOR"        = -1.60944,
                      "PR_SD"        = 0.1,
                      "PR_type"      = 0,
                      "PHASE"        = -4,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
row.names(MG_parms1)<-"NatM"
##--------------------------------------------------------------
MG_parms2<-data.frame("LO"           = 2,
                      "HI"           = 15,
                      "INIT"         = 5,
                      "PRIOR"        = 32,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -5,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
row.names(MG_parms2)<-"L_at_Amin" 
##--------------------------------------------------------------
MG_parms3<-data.frame("LO"           = 45,
                      "HI"           = 60,
                      "INIT"         = 53,
                      "PRIOR"        = 50,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -3,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
row.names(MG_parms3)<-"L_at_Amax"
##--------------------------------------------------------------
MG_parms4<-data.frame("LO"           = 0.2,
                      "HI"           = 0.4,
                      "INIT"         = 0.3,
                      "PRIOR"        = 0.3,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -3,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
row.names(MG_parms4)<-"VonBert_K"
##--------------------------------------------------------------
MG_parms5<-data.frame("LO"           = 0.03,
                      "HI"           = 0.16,
                      "INIT"         = 0.066,
                      "PRIOR"        = 0.1,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -5,
"env_var&link"=0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"=0,"Block"=0,"Block_Fxn"=0)
row.names(MG_parms5)<-"CV_young"
##--------------------------------------------------------------
MG_parms6<-data.frame("LO"           = 0.03,
                      "HI"           = 0.16,
                      "INIT"         = 0.062,
                      "PRIOR"        = 0.1,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -5,
"env_var&link"=0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"=0,"Block"=0,"Block_Fxn"=0)
row.names(MG_parms6)<-"CV_old"
##--------------------------------------------------------------
MG_parms7<-data.frame("LO"           = -3,
                      "HI"           = 3,
                      "INIT"         = 7e-06,
                      "PRIOR"        = 7e-06,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -50,
"env_var&link"=0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
row.names(MG_parms7)<-"Wtlen_1"
##--------------------------------------------------------------
MG_parms8<-data.frame("LO"           = -3,
                      "HI"           = 3,
                      "INIT"         = 2.9624,
                      "PRIOR"        = 2.9624,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -50,
"env_var&link"=0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"=0,"Block"=0,"Block_Fxn"=0)
row.names(MG_parms8)<-"Wtlen_2"
##--------------------------------------------------------------
MG_parms9<-data.frame("LO"           = -3,
                      "HI"           = 43,
                      "INIT"         = 37,
                      "PRIOR"        = 37,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -50,
"env_var&link" = 0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"=0,"Block"=0,"Block_Fxn"= 0)
row.names(MG_parms9)<-"Mat50%"
##--------------------------------------------------------------
MG_parms10<-data.frame("LO"           = -3,
                       "HI"           = 3,
                       "INIT"         = -0.48,
                       "PRIOR"        = -0.48,
                       "PR_SD"        = 99,
                       "PR_type"      = 0,
                       "PHASE"        = -50,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
row.names(MG_parms10)<-"Mat_slope"

#----------------------------------------------------------------------
MG_parms0<-data.frame("LO"="biological Parameters ","HI"=" ","INIT"=" ","PRIOR"=" ","PR_SD"=" ","PR_type"=" ",
                      "PHASE"=" ","env_var&link"=" ","dev_link"= " ","dev_minyr"=" ",
                      "dev_maxyr"=" ","dev_PH"=" ","Block"=" ","Block_Fxn"=" ");row.names(MG_parms0)<-""
MG_parms<-rbind(MG_parms0,MG_parms1,MG_parms2,MG_parms3,MG_parms4,MG_parms5,
                MG_parms6,MG_parms7,MG_parms8,MG_parms9,MG_parms10)
namesBioPar<-row.names(MG_parms)
MG_parms$type<-namesBioPar

#----------------------------------------------------------------------

####################################################
# RELACION STOCK RECLUTA ----
####################################################

SR_parms1<-data.frame("LO"           = 5,
                      "HI"           = 30,
                      "INIT"         = 20,
                      "PRIOR"        = 15,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = 1,
                      "env_var&link"=0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"=0,"Block"=0,"Block_Fxn"=0)
rownames(SR_parms1)<-"SR_LN(R0)"
##--------------------------------------------------------------
SR_parms2<-data.frame("LO"           = 0.2,
                      "HI"           = 1,
                      "INIT"         = 0.88,
                      "PRIOR"        = 0.777,
                      "PR_SD"        = 0.113,
                      "PR_type"      = 2,
                      "PHASE"        = -4,
                      "env_var&link" = 0,"dev_link"=0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(SR_parms2)<-"SR_SCAA_null"
##--------------------------------------------------------------
SR_parms3<-data.frame("LO"           = 0.3,
                      "HI"           = 1.6,
                      "INIT"         = 0.6,
                      "PRIOR"        = 1.1,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -6,
                      "env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(SR_parms3)<-"SR_sigmaR"
##--------------------------------------------------------------
SR_parms4<-data.frame("LO"           = -5,
                      "HI"           = 5,
                      "INIT"         = 0,
                      "PRIOR"        = 0,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -50,
                      "env_var&link"=0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(SR_parms4)<-"SR_regime"
##--------------------------------------------------------------
SR_parms5<-data.frame("LO"           = 0,
                      "HI"           = 2,
                      "INIT"         = 0,
                      "PRIOR"        = 1,
                      "PR_SD"        = 99,
                      "PR_type"      = 0,
                      "PHASE"        = -50,
                      "env_var&link"=0,"dev_link"=0,"dev_minyr"=0,"dev_maxyr"=0,"dev_PH"=0,"Block"=0,"Block_Fxn"=0)
rownames(SR_parms5)<-"SR_autocorr"

####################################################
SR_parms0<-data.frame("LO"="Stock-recruitment relationship ","HI"=" ","INIT"=" ","PRIOR"=" ","PR_SD"=" ","PR_type"=" ",
                      "PHASE"=" ","env_var&link"=" ","dev_link"= " ","dev_minyr"=" ",
                      "dev_maxyr"=" ","dev_PH"=" ","Block"=" ","Block_Fxn"=" ");row.names(SR_parms0)<-""
SR_parms<-rbind(SR_parms0,SR_parms1,SR_parms2,SR_parms3,SR_parms4,SR_parms5)
namesSRPar<-row.names(SR_parms)
SR_parms$type<-namesSRPar

####################################################
# CAPTURABILIDAD ----
####################################################
#--------------------------------------------------
Q_parms1<-data.frame( "LO"           = -7,
                      "HI"           = 5,
                      "INIT"         = 0.516018,
                      "PRIOR"        = 0,
                      "PR_SD"        = 1,
                      "PR_type"      = 0,
                      "PHASE"        = 1,
                      "env_var&link" = 0,
                      "dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(Q_parms1)<-"LnQ_base_SURVEY1"
#--------------------------------------------------
Q_parms0<-data.frame("LO"="Catchability ","HI"=" ","INIT"=" ","PRIOR"=" ","PR_SD"=" ","PR_type"=" ",
                      "PHASE"=" ","env_var&link"=" ","dev_link"= " ","dev_minyr"=" ",
                      "dev_maxyr"=" ","dev_PH"=" ","Block"=" ","Block_Fxn"=" ");row.names(Q_parms0)<-""
Q_parms<-rbind(Q_parms0,Q_parms1)
namesQPar<-row.names(Q_parms)
Q_parms$type<-namesQPar


####################################################
# SELECTIVIDAD POR TALLAS ----
####################################################
size_selex_parms1<-data.frame(
  "LO"           = -1,
  "HI"           = 20,
  "INIT"         = 12.6, 
  "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
  "PR_SD"        = 0,#este valor es ignorado si PR_type=0
  "PR_type"      = 0, #0=no se usa (none)
  "PHASE"        = 2,#se estima
  "env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(size_selex_parms1)<-"SizeSel_P_1_Flota(1)"

size_selex_parms2<-data.frame(
  "LO"           = -1,
  "HI"           = 3,
  "INIT"         = 0.193, 
  "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
  "PR_SD"        = 0,#este valor es ignorado si PR_type=0
  "PR_type"      = 0, #0=no se usa (none)
  "PHASE"        = 2,#se estima
  "env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(size_selex_parms2)<-"SizeSel_P_2_Flota(1)"

size_selex_parms3<-data.frame(
  "LO"           = -3,
  "HI"           = 20,
  "INIT"         = 14.3, 
  "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
  "PR_SD"        = 0,#este valor es ignorado si PR_type=0
  "PR_type"      = 0, #0=no se usa (none)
  "PHASE"        = 3,#se estima
  "env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(size_selex_parms3)<-"SizeSel_P_1_SURVEY1(2)"

size_selex_parms4<-data.frame(
  "LO"           = -3,
  "HI"           = 3,
  "INIT"         = 0.406, 
  "PRIOR"        = 0, # valor esperado ignorado si PR_type=0
  "PR_SD"        = 0,#este valor es ignorado si PR_type=0
  "PR_type"      = 0, #0=no se usa (none)
  "PHASE"        = 3,#se estima
  "env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(size_selex_parms4)<-"SizeSel_P_2_SURVEY1(2)"
#----------------------------------------------------------------------
size_selex_parms0<-data.frame("LO"="Size selectivity ","HI"=" ","INIT"=" ","PRIOR"=" ","PR_SD"=" ","PR_type"=" ",
                     "PHASE"=" ","env_var&link"=" ","dev_link"= " ","dev_minyr"=" ",
                     "dev_maxyr"=" ","dev_PH"=" ","Block"=" ","Block_Fxn"=" ");row.names(size_selex_parms0)<-""
size_selex_parms<-rbind(size_selex_parms0,size_selex_parms1,size_selex_parms2,size_selex_parms3,size_selex_parms4)
namesSelfPar<-row.names(size_selex_parms)
size_selex_parms$type<-namesSelfPar

####################################################
# SELECTIVIDAD POR EDAD ----
####################################################
age_selex_parms1<-data.frame("LO"            = 0,
                             "HI"            = 40,
                             "INIT"          = 0,
                             "PRIOR"         = 5,
                             "PR_SD"         = 99,
                             "PR_type"       = 0,
                             "PHASE"         = -99,
"env_var&link"  = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(age_selex_parms1)<-"AgeSel_P_1_FISHERY(1)"
#--------------------------------------------------
age_selex_parms2<-data.frame("LO"           = 0,
                             "HI"           = 40,
                             "INIT"         = 40,
                             "PRIOR"        = 6,
                             "PR_SD"        = 99,
                             "PR_type"      = 0,
                             "PHASE"        = -99,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(age_selex_parms2)<-"AgeSel_P_2_FISHERY(1)"
#--------------------------------------------------
age_selex_parms3<-data.frame(row.names="AgeSel_P_1_SURVEY1(2)",
                             "LO"           = 0,
                             "HI"           = 40,
                             "INIT"         = 0,
                             "PRIOR"        = 5,
                             "PR_SD"        = 99,
                             "PR_type"      = 0,
                             "PHASE"        = -99,
"env_var&link"= 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0, "Block"= 0,"Block_Fxn"= 0)
rownames(age_selex_parms3)<-"AgeSel_P_1_SURVEY1(2)"
#--------------------------------------------------
age_selex_parms4<-data.frame(row.names="AgeSel_P_2_SURVEY1(2)",
                             "LO"           = 0,
                             "HI"           = 40,
                             "INIT"         = 40,
                             "PRIOR"        = 6,
                             "PR_SD"        = 99,
                             "PR_type"      = 0,
                             "PHASE"        = -99,
"env_var&link" = 0,"dev_link"= 0,"dev_minyr"= 0,"dev_maxyr"= 0,"dev_PH"= 0,"Block"= 0,"Block_Fxn"= 0)
rownames(age_selex_parms4)<-"AgeSel_P_2_SURVEY1(2)"
#----------------------------------------------------------------------
age_selex_parms0<-data.frame("LO"="Age selectivity ","HI"=" ","INIT"=" ","PRIOR"=" ","PR_SD"=" ","PR_type"=" ",
                              "PHASE"=" ","env_var&link"=" ","dev_link"= " ","dev_minyr"=" ",
                              "dev_maxyr"=" ","dev_PH"=" ","Block"=" ","Block_Fxn"=" ");row.names(age_selex_parms0)<-""
age_selex_parms<-rbind(age_selex_parms0,age_selex_parms1,age_selex_parms2,age_selex_parms3,age_selex_parms4)
namesSelaPar<-row.names(age_selex_parms)
age_selex_parms$type<-namesSelaPar


#----------------------------------------------------------------------
Parametros<-rbind(MG_parms,SR_parms,Q_parms,size_selex_parms,age_selex_parms)
#----------------------------------------------------------------------

# Guardar archivo excell con datos requeridos para SS3

wb <- createWorkbook()
addWorksheet(wb, "Catch")
writeData(wb, sheet = "Catch", x = catch1)
addWorksheet(wb, "Survey")
writeData(wb, sheet = "Survey", x = CPUE1)
addWorksheet(wb, "AgeComp_Survey")
writeData(wb, sheet = "AgeComp_Survey", x = new_agecomp_survey)
addWorksheet(wb, "AgeComp_Catch")
writeData(wb, sheet = "AgeComp_Catch", x = new_agecomp_catch)
addWorksheet(wb, "LenghComp_Catch")
writeData(wb, sheet = "LenghComp_Catch", x = new_lencomp1)
addWorksheet(wb, "LenghComp_Survey")
writeData(wb, sheet = "LenghComp_Survey", x = new_lencomp2)
addWorksheet(wb, "Watage")
writeData(wb, sheet = "Watage", x = wtat1)
addWorksheet(wb, "Parameters")
writeData(wb, sheet = "Parameters", x = Parametros)

#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
# Guardar el archivo Excel
#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
saveWorkbook(wb, dir.excell,overwrite = TRUE)


#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
# Gerena Rdata ----
#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
# save(list=ls(all=T),
#      file=paste(here("Archivos_datos",'Merluzas_a4a'),
#                      "/Rdata_a4a.RData",sep=""))
#≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠
