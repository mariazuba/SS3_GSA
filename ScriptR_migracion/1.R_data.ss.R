##############################################################################################
### Migración de a4a a SS3:  ----
##############################################################################################
library(here)
#===============================================================================
# CARGAR LIBRERIAS ----
#===============================================================================
source(here("ScriptR_migracion","Librerias_Requeridas.R")) 
#===============================================================================
# CARGAR DIRECTORIOS ----
#===============================================================================
source(here("ScriptR_migracion","Directorios_Requeridos.R")) 

#===============================================================================
# LEER ARCHIVOS DE DATOS SS3 - MODELO BASE SIMPLE ----
#===============================================================================
dat <- r4ss::SS_readdat(here(dir.base,"data.ss")) 
# Lista para modificar
dat1<-dat 
# Contenido de la lista
names(dat1) 

#===============================================================================
# CARGAR RDATA: DATOS TRANSFORMADOS DE a4a EN FORMATO SS3 ----
#===============================================================================
load(here("Archivos_datos",'Merluzas_a4a',"Rdata_a4a.RData"))

#===============================================================================
# MODIFICAR ARCHIVO data.ss  ----
#===============================================================================

# Se incorpora datos transformados de a4a en formato SS3

#===============================================================================
# Especificaciones iniciales ----
#===============================================================================
dat1$Comments       <-"#C 2022 modelo GSA6 data file"
dat1$styr           <- 2003
dat1$endyr          <- 2021
dat1$nseas          <- 1  
dat1$months_per_seas<- 12      
dat1$Nsubseasons    <- 2 #  igual al base    
dat1$spawn_month    <- 6 # it is assumed in July
dat1$Ngenders       <- 1 
dat1$Nages          <- 6
dat1$N_areas        <- 1    
dat1$Nfleets        <- 2 
#===============================================================================
# Especificaciones de la flota comercial y campañas ----
#===============================================================================
# Options required by SS3

fleetnames1    <-  c("FISHERY", "SURVEY1") 
type_seine     <-  1  # Fleet with input catches
type_survey    <-  3  # Assumes no catch removals even if associated catches are specified below.
timming_catch  <- -1  # Catch is treated as if it occurred over the whole season
timming_survey <-  1  # The fleet timing is not used and only the month value associated with each observation is relevant
units_catch    <-  1  # Biomass
units_survey   <-  2  # Biomass (in metric tons) 
area_catch     <-  1  
area_survey    <-  1

fleetinfo1 <- data.frame(type            = c(type_seine,type_survey), # type_fleet (type_seine and type_survey) Options required by SS3
                         surveytiming    = c(timming_catch,timming_survey), # timing (timming_catch and timming_survey) Options required by SS3
                         area            = c(area_catch,area_survey), 
                         units           = c(units_catch,units_survey), # units (units_catch and units_survey) Options required by SS3
                         need_catch_mult = c(0,0), # from file "simple", dir.base SS3 
                         fleetname       = fleetnames1) 
dat1$fleetinfo <- fleetinfo1     

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
#---------------------------------------------------------------------------------
dat1$catch<-catch1 

#===============================================================================
# Especificaciones de los índices de abundancia ----
#===============================================================================
CPUEinfo_Fleet     <- c(1,2) # se enumeran las flotas en el mismo orden que cuando se especificaron los tipos de flota
CPUEinfo_Units     <- c(1,1) # 0=número y 1=biomasa
CPUEinfo_Errtype   <- c(0,0) # 0=lognormal 
CPUEinfo_SD_Report <- c(0,0) # no sdreport
CPUEinfo_names     <-fleetnames1
# Genera data.frame
CPUEinfo1<-data.frame(Fleet     = CPUEinfo_Fleet,
                      Units     = CPUEinfo_Units,
                      Errtype   = CPUEinfo_Errtype,
                      SD_Report = CPUEinfo_SD_Report)
row.names(CPUEinfo1)<-CPUEinfo_names
dat1$CPUEinfo<-CPUEinfo1 
#===============================================================================
# Datos de índices de abundancia ----
#===============================================================================
yearf1<-2003:2021
CPUE_year<-c(yearf1)
CPUE_seas  <-rep(1,length(yearf1)) 

CPUE_index <-c(rep(2,length(yearf1))) 
CPUE_obs   <-c(TUNEFF$total) # migra desde a4a
CPUE_se_log<-c(rep(0.3,length(yearf1)),
               rep(0.3,length(yearf1))) 
# Genera data.frame
CPUE1<-data.frame(year   = CPUE_year, #  años
                  seas   = CPUE_seas, # temporada/mes
                  index  = CPUE_index, # número de cada flota 
                  obs    = CPUE_obs,    # valor observado
                  se_log = CPUE_se_log) # coeficiente de variación asumido en 0.3
dat1$CPUE<-CPUE1 

#===============================================================================
# 1. Descartes ----
#===============================================================================
dat1$N_discard_fleets<-dat$N_discard_fleets   # from file "simple", dir.base SS3 
#===============================================================================
# 2. Tallas medias ----
#===============================================================================
dat1$use_meanbodywt<-dat$use_meanbodywt # from file "simple", dir.base SS3 
#------------------------------------------------------------------------------

#===============================================================================
# 1. Especificaciones de Bins tallas ----
#===============================================================================
dat1$lbin_method  <-2     # opción 2= genera lbin a partir de binwidth, minimum_size y maximum_size
dat1$binwidth     <-2   # cada 2
dat1$minimum_size <-10     # desde L=10
dat1$maximum_size <-70  # hasta L=70
dat1$use_lencomp  <-0     #  0= no se usa datos de tallas

#===============================================================================
# 2. Especificación composición de tallas ----
#===============================================================================
nfleets<-dat1$Nfleets
len_info_mintailcomp   <-rep(-1,nfleets)
len_info_addtocomp     <-rep(0.001,nfleets)
len_info_combine_M_F   <-rep(0,nfleets)
len_info_CompressBins  <-rep(0,nfleets)
len_info_CompError     <-rep(0,nfleets)
len_info_ParmSelect    <-rep(0,nfleets)
len_info_minsamplesize <-rep(1,nfleets)
# Genera data.frame
len_info1<-data.frame(mintailcomp   = len_info_mintailcomp,
                      addtocomp     = len_info_addtocomp,
                      combine_M_F   = len_info_combine_M_F,
                      CompressBins  = len_info_CompressBins,
                      CompError     = len_info_CompError,
                      ParmSelect    = len_info_ParmSelect,
                      minsamplesize = len_info_minsamplesize)
row.names(len_info1)<-fleetnames1 
dat1$len_info<-len_info1

#===============================================================================
# 3. Especificación del vector de tallas ----
#===============================================================================
dat1$N_lbins<-26  
dat1$lbin_vector<-seq(20,70,2) 

#===============================================================================
# 4. Datos de composición de tallas ----
#===============================================================================
tallasplot<-merge(tallasflota,
            merge(tallaspelago,
                  tallasecocadiz,
                  by=c('year','step','area','age','length',"text"),all=TRUE),
                  by=c('year','step','area','age','length',"text"),all=TRUE)%>% 
           select(-area, -age,-text)%>% 
           magrittr::set_colnames(
           c('year','step','length','Flota','Pelago','Ecocadiz'))%>%
           mutate(Gender=0,
           Part=0,
           Nsamp=100) %>%
           melt(id.vars=c('year','step','length','Gender','Part','Nsamp'))%>% 
           spread(length,value) 

tallasSS3<-tallasplot[order(tallasplot$variable),]
tallasSS3[is.na(tallasSS3)] <- 0              
nyear<-length(tallasSS3$year)/3

new_lencomp <- data.frame(Yr     = tallasSS3$year, 
                          Seas   = tallasSS3$step, 
                          FltSvy = c(rep(1,nyear),rep(2,nyear),rep(3,nyear)), 
                          Gender = tallasSS3$Gender, 
                          Part   = tallasSS3$Part, 
                          Nsamp  = tallasSS3$Nsamp)

dat_rows_names <- paste("L",seq(3.5,21.5,0.5) ,sep="")

dat_rows <- tallasSS3[,7:43]
names(dat_rows)<-dat_rows_names 
new_lencomp1<-cbind(new_lencomp, dat_rows)

dat1$lencomp<-new_lencomp1 

#===============================================================================
#  Datos de composición de edad ----
#===============================================================================
#===============================================================================
# 1. Bins de edad ----
#===============================================================================
dat1$N_agebins<-0 # 0 = No se usan datos de edad
dat1$agebin_vector<-NULL # No se ingresa información = NULL
#===============================================================================
# 2. Datos de error de lectura de edad ----
#===============================================================================
dat1$N_ageerror_definitions<- NULL # No se ingresa información = NULL
dat1$ageerror<-NULL # No se ingresa información = NULL
#===============================================================================
# 3. Especificaciones de los datos de composicion de edad ----
#===============================================================================
dat1$age_info<-NULL # No se ingresa información = NULL
#===============================================================================
# 4. Datos de composicion de edad ----
#===============================================================================
dat1$Lbin_method <- NULL # No se ingresa información = NULL
dat1$agecomp<-NULL # No se ingresa información = NULL

#===============================================================================
# 5. Tallas medias a la edad ----
#===============================================================================
dat1$use_MeanSize_at_Age_obs <- 0 # 0 = no se usa datos de tallas medias a la edad
# Genera data.frame
# MeanSize_at_Age_obs_esp<-data.frame(Yr    =0,
#                                     Seas  =0,
#                                     FltSvy=0,
#                                     Gender=0,
#                                     Part  =0,
#                                     AgeErr=0,
#                                     Ignore=0)
# 
# dat_rows_names <- c(paste("L",seq(0,5,1),sep=""),
#                     paste("nmL",seq(0,5,1),sep="")) 
# dat_rows <- as.data.frame(matrix(data = 0, 
#                                  nrow = nrow(MeanSize_at_Age_obs_esp), 
#                                  ncol = length(dat_rows_names)))
# names(dat_rows)<-dat_rows_names 
# # crear data.frame 
# MeanSize_at_Age_obs1 <-cbind(MeanSize_at_Age_obs_esp, dat_rows)

# Descomentar código previo para modificar MeanSize_at_Age_obs
# No se ingresa información = NULL
dat1$MeanSize_at_Age_obs<-NULL #MeanSize_at_Age_obs1

#===============================================================================
# Datos ambientales ----
#===============================================================================
dat1$N_environ_variables<-dat$N_environ_variables #0 # from file "simple", dir.base SS3 
#===============================================================================
# Sizefreq data  ----
#===============================================================================
dat1$N_sizefreq_methods<-dat$N_sizefreq_methods #0 # from file "simple", dir.base SS3 
#===============================================================================
# Datos de marcaje-recaptura ----
#===============================================================================
dat1$do_tags<-dat$do_tags #0 # from file "simple", dir.base SS3 
#===============================================================================
# Datos de morfos  ----
#===============================================================================
dat1$morphcomp_data<-dat$morphcomp_data #0 # from file "simple", dir.base SS3 
#===============================================================================
# Priors de selectividad ----
#===============================================================================
dat1$use_selectivity_priors<-dat$use_selectivity_priors #0 # from file "simple", dir.base SS3 
#===============================================================================
# Final de archivo data.ss ----
#===============================================================================
dat1$eof<-TRUE # Options required by SS3

#------------------------------------------------------------------------------
# ESCRIBIR ARCHIVO MODIFICADO data.ss ----
#------------------------------------------------------------------------------
r4ss::SS_writedat(dat1,
                  outfile=here(dir.mod,"data_GSA6.ss"),
                  overwrite = TRUE)
#------------------------------------------------------------------------------

