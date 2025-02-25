#Genera archivos SS3

library(r4ss)
library(reshape2)
library(readxl)
library(openxlsx)
library(dplyr)
#Download and unzip the models folder stored on GitHub within the nmfs-ost/ss3-test-models repository.
#download_models(dir = getwd())
model_name <- list.files("models") # see the model names
model_name 
mod_path <- "models/Hake_2018"
inputs <- r4ss::SS_read(dir = mod_path)

data_path <- "datos_Hake_SS3.xlsx" # data in SS3 format

# Genera excell con data como ejemplo de matrices de datos requeridos
wb <- createWorkbook()
addWorksheet(wb, "LenghComp")
writeData(wb, sheet = "LenghComp", x = inputs$dat$lencomp)
addWorksheet(wb, "AgeComp")
writeData(wb, sheet = "AgeComp", x = inputs$dat$agecomp)
addWorksheet(wb, "Bioparms")
writeData(wb, sheet = "Bioparms", x = inputs$ctl$MG_parms,rowNames = T)
addWorksheet(wb, "SR_parms")
writeData(wb, sheet = "SR_parms", x = inputs$ctl$SR_parms,rowNames = T)
addWorksheet(wb, "Qparms")
writeData(wb, sheet = "Qparms", x = inputs$ctl$Q_parms,rowNames = T)
addWorksheet(wb, "Qoption")
writeData(wb, sheet = "Qoption", x = inputs$ctl$Q_options,rowNames = T)
addWorksheet(wb, "size_selex_parms")
writeData(wb, sheet = "size_selex_parms", x = inputs$ctl$size_selex_parms,rowNames = T)
addWorksheet(wb, "size_selex_type")
writeData(wb, sheet = "size_selex_type", x = inputs$ctl$size_selex_types,rowNames = T)
addWorksheet(wb, "age_selex_parms")
writeData(wb, sheet = "age_selex_parms", x = inputs$ctl$age_selex_parms,rowNames = T)
addWorksheet(wb, "age_selex_type")
writeData(wb, sheet = "age_selex_type", x = inputs$ctl$age_selex_types,rowNames = T)
addWorksheet(wb, "wtatage")
writeData(wb, sheet = "wtatage", x = inputs$wtatage)
saveWorkbook(wb, "DataSS3Hakebase.xlsx",overwrite = TRUE)

#'*###############################################################################*
# DATA FILE ----
#'*###############################################################################*

inputs$dat$Comments <- sprintf("#C %s model generated by Hake", mod_path)

## General specification ----
inputs$dat$styr<-1966
inputs$dat$endyr<-2017
inputs$dat$Nsexes<-1
inputs$dat$Nages<-20
inputs$dat$Nfleets<-2
inputs$dat$fleetnames<-c("Fishery","Survey")

#'********************
## Catches ----
#'********************
##'*Specifications* 
Fleetinfo <- readxl::read_excel(data_path, "Fleetinfo", na = c("", "NA"))
inputs$dat$fleetinfo <- inputs$dat$fleetinfo[c(),,drop = FALSE]
inputs$dat$fleetinfo <-data.frame(Fleetinfo, stringsAsFactors = TRUE)
##'*Data*
landings <- readxl::read_excel(data_path, "Catch", na = c("", "NA"))
inputs$dat$catch <- inputs$dat$catch[c(),,drop = FALSE]
inputs$dat$catch <- data.frame(landings, stringsAsFactors = TRUE)


#'********************
## Surveys ----
#'********************
##'*Specifications* 
Surveyinfo <- readxl::read_excel(data_path, "Surveyinfo", na = c("", "NA"))
inputs$dat$CPUEinfo <- inputs$dat$CPUEinfo[c(),,drop = FALSE]
inputs$dat$CPUEinfo <- data.frame(Surveyinfo, stringsAsFactors = TRUE)
##'*Data*
survey <- readxl::read_excel(data_path, "Survey", na = c("", "NA"))
inputs$dat$CPUE <- inputs$dat$CPUE[c(),,drop = FALSE]
inputs$dat$CPUE <- data.frame(survey, stringsAsFactors = TRUE)


#'********************
## Discard ----
#'********************
##'*Specifications* 
inputs$dat$N_discard_fleets<- 0
inputs$dat$discard_fleet_info<-NULL
##'*Data*
inputs$dat$discard_data<-NULL


#'********************
## Meanbodywt ----
#'********************
##'*Specifications* 
inputs$dat$use_meanbodywt <-0
inputs$dat$DF_for_meanbodywt<-NULL

inputs$dat$meanbodywt<-NULL


#'********************
## Length composition ----
#'********************
##'*Specifications*
inputs$dat$lbin_method<-2
inputs$dat$binwidth<-2
inputs$dat$minimum_size<-10
inputs$dat$maximum_size<-70
inputs$dat$use_lencomp <-0
inputs$dat$lbin_vector <-seq(inputs$dat$minimum_size,inputs$dat$maximum_size,inputs$dat$binwidth)
inputs$dat$N_lbins.    <-length(inputs$dat$lbin_vector)

Leninfo <- readxl::read_excel(data_path, "Leninfo", na = c("", "NA"))
inputs$dat$len_info <- inputs$dat$len_info[c(),,drop = FALSE]
inputs$dat$len_info <- data.frame(Leninfo, stringsAsFactors = TRUE)

##'*Data*
#inputs$dat$lencomp <- inputs$dat$lencomp[c(),,drop = FALSE]



#'********************
## Age composition ----
#'********************
##'*Specifications* 
inputs$dat$agebin_vector<-seq(1,15,1)
inputs$dat$N_agebins<-length(inputs$dat$agebin_vector)
inputs$dat$N_ageerror_definitions<-45
inputs$dat$Lbin_method<-1 #1=poplenbins; 2=datalenbins; 3=lengths

Ageerror <- readxl::read_excel(data_path, "Ageerror", na = c("", "NA"))
inputs$dat$ageerror<-inputs$dat$ageerror[c(),,drop = FALSE]
inputs$dat$ageerror <- data.frame(Ageerror, stringsAsFactors = TRUE)

Ageinfo <- readxl::read_excel(data_path, "Ageinfo", na = c("", "NA"))
inputs$dat$age_info <- inputs$dat$age_info[c(),,drop = FALSE]
inputs$dat$age_info <- data.frame(Ageinfo, stringsAsFactors = TRUE)


##'*Data*
ageComp_catch <- readxl::read_excel(data_path, "AgeComp_Catch", na = c("", "NA"))
ageComp_survey <- readxl::read_excel(data_path, "AgeComp_Survey", na = c("", "NA"))
inputs$dat$agecomp <- inputs$dat$agecomp[c(),,drop = FALSE]
inputs$dat$agecomp <- data.frame(rbind(ageComp_catch,ageComp_survey), stringsAsFactors = TRUE)


#'********************
## MeanSize_at_Age_obs ----
#'********************
inputs$dat$MeanSize_at_Age_obs <- NULL


#'*###############################################################################*
# CONTROL FILE ----
#'*###############################################################################*
#'Growth 
inputs$ctl$Growth_Age_for_L1<-1
inputs$ctl$Growth_Age_for_L2<-20
#'*------------------------------------------------------*
## Biological parameter ----
## Create biological params ----
#These parameters could be requested as input data (LO,HI,INIT)
#all parameters are fixed (PHASE=-3), that is, they are not estimated by the model.
 MGparm<- readxl::read_excel(data_path, "Bioparm", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$MG_parms <- inputs$ctl$MG_parms[c(),,drop = FALSE]
 inputs$ctl$MG_parms <-  MGparm[,1:14] 
 row.names(inputs$ctl$MG_parms)<- MGparm[,15]
 #'*------------------------------------------------------*
 ## Spawner_Recruitment ----
 ### Specification ----
 inputs$ctl$SR_function <- 3
 ### Parameters ----
 SRparms<- readxl::read_excel(data_path, "SRparm", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$SR_parms <- inputs$ctl$SR_parms[c(),,drop = FALSE]
 inputs$ctl$SR_parms <-  SRparms[,1:14] 
 row.names(inputs$ctl$SR_parms)<-  SRparms[,15]
 
 #'*------------------------------------------------------*
 ## Create recruitment deviation (recDev) ----
 inputs$ctl$MainRdevYrFirst<-1970#inputs$dat$styr
 inputs$ctl$MainRdevYrLast<-2016#inputs$dat$endyr  
 inputs$ctl$recdev_phase<-1
 inputs$ctl$recdev_adv<-1#0
 #'*------------------------------------------------------*
 ## Create fishing Mortality (F) ----
 inputs$ctl$F_ballpark_year<- -1999# -inputs$dat$styr
 inputs$ctl$init_F<-NULL
 #'*------------------------------------------------------*
 
 #'*------------------------------------------------------*
 ## Catchability ----
 ### Specification ----
 Qoption<- readxl::read_excel(data_path, "Qoption", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$Q_options <- inputs$ctl$Q_options[c(),,drop = FALSE]
 inputs$ctl$Q_options <- Qoption[,1:6] 
 row.names(inputs$ctl$Q_options)<- Qoption[,7]
 
 ### Parameters ----
 Qparms<- readxl::read_excel(data_path, "Qparm", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$Q_parms <- inputs$ctl$Q_parms[c(),,drop = FALSE]
 inputs$ctl$Q_parms <- Qparms[,1:14] 
 row.names(inputs$ctl$Q_parms)<- Qparms[,15]
 
 #'*------------------------------------------------------*
 ## Selectivity ----
 #'Length Selectivity options 
 Sel_len_type<- readxl::read_excel(data_path, "Sel_len_type", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$size_selex_types <- inputs$ctl$size_selex_types[c(),,drop = FALSE]
 inputs$ctl$size_selex_types <- Sel_len_type[,1:4] 
 row.names(inputs$ctl$size_selex_types)<- Sel_len_type[,5]
 #'Age Selectivity options  
 Sel_age_type<- readxl::read_excel(data_path, "Sel_age_type", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$age_selex_types <- inputs$ctl$age_selex_types[c(),,drop = FALSE]
 inputs$ctl$age_selex_types <- Sel_age_type[,1:4] 
 row.names(inputs$ctl$age_selex_types)<- Sel_age_type[,5]
 #'Age Selectivity params
 Selparms<- readxl::read_excel(data_path, "Selparm", na = c("", "NA")) %>% as.data.frame()
 inputs$ctl$age_selex_parms <- inputs$ctl$age_selex_parms[c(),,drop = FALSE]
 inputs$ctl$age_selex_parms <- Selparms[,1:14] 
 row.names(inputs$ctl$age_selex_parms)<- Selparms[,15]
 
 #'Dirichlet parameters
 inputs$ctl$dirichlet_parms <-NULL

 #'*------------------------------------------------------*
 # Lambdas ----
 # inputs$ctl$lambdas <- inputs$ctl$lambdas[c(),,drop = FALSE]
 inputs$ctl$lambdas<-NULL
 inputs$ctl$maxlambdaphase<- 1
 inputs$ctl$N_lambdas<-0
 inputs$ctl$more_stddev_reporting<-0
 
 #'*###############################################################################*
 # WTATAGE FILE ----
 #'*###############################################################################*
 
 wtatage <- readxl::read_excel(data_path, "Watage", na = c("", "NA"))
 inputs$wtatage <-  inputs$wtatage[c(),,drop = FALSE]
 inputs$wtatage <- data.frame(wtatage, stringsAsFactors = TRUE)
 inputs$wtatage[is.na(wtatage)]<-0

 #'*###############################################################################*
 # STARTER FILE ----
 #'*###############################################################################*
 # Por ahora no hago cambios en este archivo
 # inputs$start
 
 #'*###############################################################################*
 # FORECAST FILE ----
 #'*###############################################################################*
 #revisar si estos años corresponden
 # inputs$fore$FirstYear_for_caps_and_allocations<-2022
 # inputs$fore$Ydecl<-2002
 # inputs$fore$Yinit<-2002 
  
#' ## Write back to mod_path ----
mod_path_new<-"SS3_model_Hake"
dir.create(path=mod_path_new, showWarnings = TRUE, recursive = TRUE)
r4ss::SS_write(inputs, dir = mod_path_new, overwrite = TRUE)
#' 
#' #' #'Ejecutar el modelo ----
#' #' #'*Entramos en el directorio desde la terminal*
system(paste0("cd ",mod_path_new,"/"))
#' #' 
#' #' #'*Permitimos que el ordenador pueda abrir el ejecutable (terminal)*
r4ss::get_ss3_exe(dir = mod_path_new, version = "v3.30.19")
system(paste0("chmod 755 ",mod_path_new,"/ss3"))
#' #' 
#' #' #'* Ejecutar el modelo SS3 usando la función run_SS_models* ----
r4ss::run(dir=mod_path_new, extras = "-nohess",exe="ss3", skipfinished=FALSE, show_in_console =T)



