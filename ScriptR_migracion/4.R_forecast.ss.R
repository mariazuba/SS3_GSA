###############################################################################
### Migración de a4a a SS3:  - Archivo forecast.ss ----
###############################################################################
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
# Leer archivo forecast.ss ----
#===============================================================================
inputs <- r4ss::SS_read(dir = dir.base)
fore<-inputs$fore # lista base
#===============================================================================
# Modificar archivo forecast.ss  ----
#===============================================================================

fore1<-fore # lista modificada

#===============================================================================
# Especificaciones  ----
#===============================================================================
fore1$benchmarks <- fore$benchmarks #1=calculate Fspr, Fbtarget, and Fmsy; Benchamarks/referene points
fore1$MSY        <- fore$MSY #2=calculate Fmsy; specifies basis for calculating a single population level Fmsy value 
fore1$SPRtarget  <- fore$SPRtarget #0.4=SPRtarget; SS3 searches for F multiplier that will produce 
                                                  # this level of spawning biomass per recruit 
                                                  # (reproductive output) relative to unfished value.
fore1$Btarget    <- fore$Btarget #0.55=Relative Biomass Target; SS3 searches for F multiplier that
                                                  # will produce this level of spawning biomass relative
                                                  # to unfished value. This is not "per recruit" and 
                                                  # takes into account the spawner-recruitment relationship
#---------------------------------------
Bmark_years1<-data.frame(matrix(rep(0,10),nrow=1,ncol=10)); colnames(Bmark_years1) <-paste("#_Bmark_years_",seq(1,10,1),sep="")

fore1$Bmark_years      <- Bmark_years1 # 0 0 0 0 0 0 0 0 0 0 ; <=0 year relative to end year; Requires 10 values...(página 23)
#---------------------------------------
fore1$Bmark_relF_Basis <-fore$Bmark_relF_Basis #1 = use year range; The specification does not affect year range for selectivity and biology.
fore1$Forecast         <-fore$Forecast #2 = use Fmsy; This input is required but is ignored if benchmarks are turned off...(página 24)
fore1$Nforecastyrs     <-fore$Nforecastyrs #1 = At least one forecast year now required if the Forecast option above is >=0 ...(página 24)
fore1$F_scalar         <-fore$F_scalar #1 = Only used if Forecast option =5 (input annual F scalar), but is a required line in the forecast file ...(página 24)
#---------------------------------------
Fcast_years1<-data.frame(matrix(c(-5,0,-5,0,-999,0),nrow=1,ncol=6)); colnames(Fcast_years1)<-paste("#_Fcast_years_",seq(1,6,1),sep="")
# Fcast_years1; se puede utilizar si se quiere modificar el vector
fore1$Fcast_years       <-fore$Fcast_years # 0 0 -10 0 -999 0 = Requires 6 values (página 24)
                                           # -999 start year; >0=absolute year; <=0 =year relative to end year; 
#---------------------------------------
fore1$Fcast_selex       <-fore$Fcast_selex #0=forecast selectivity is mean from year range; (página 26)
                                           # Determines the selectivity used in the forecast years 
fore1$ControlRuleMethod <-fore$ControlRuleMethod #1=catch as function of SSB, buffer on F; (página 26)
                                                 # Used to apply reductions ("buffer") to either 
                                                 # the catch or F based on the control rule 
                                                 # during the forescast period 
fore1$BforconstantF     <-fore$BforconstantF #0.4=relative biomass level to unfished biomass above which F is constante at control rule F.
fore1$BfornoF           <-fore$BfornoF #0.1=relative biomass leves to unfished biomass below which F is set to 0 (management threshold)
fore1$Flimitfraction    <-fore$Flimitfraction #1=would set catch equal to the overfishing limit; (multiplier between 0-1 or -1, página 27) 
fore1$N_forecast_loops  <-fore$N_forecast_loops #2=Number of forecast loops, 2= ABC control rule; SS3 sequentially goes through the forecast up to three times (página 27)
fore1$First_forecast_loop_with_stochastic_recruitment<- fore$First_forecast_loop_with_stochastic_recruitment #3=first forecast loop with stochastic recruitment 
fore1$fcast_rec_option  <-fore$fcast_rec_option #1=value*(spawner recruit curve) 
fore1$fcast_rec_val     <-fore$fcast_rec_val #1= scalar or number of years of recent main recruitments to average
fore1$Forecast_loop_control_5 <-fore$Forecast_loop_control_5#0=Reserved for future model features
fore1$FirstYear_for_caps_and_allocations <-2023#first year for caps and allocations
fore1$stddev_of_log_catch_ratio <-fore$stddev_of_log_catch_ratio#0=implementation error;  the SD of the log of the ratio between the realized catch and the target catch in the forecast.
fore1$Do_West_Coast_gfish_rebuilder_output <-fore$Do_West_Coast_gfish_rebuilder_output#0=omit U.S. West Coast rebuilder output (página 29)
fore1$Ydecl <-1999 # rebuilder catch (Year declared)
fore1$Yinit <-2002 # rebuilder start year (Year Initial)
fore1$fleet_relative_F <-fore$fleet_relative_F#1=use first-last allocation year
fore1$basis_for_fcast_catch_tuning <-fore$basis_for_fcast_catch_tuning#2=total catch biomass; Basis for maximum forecast catch:
#---------------------------------------
# enter list of fleet number and 
# allocation group assignment, 
# if any; terminate with fleet=-9999
fleet.as.all <-data.frame(Fleet=1,Group=1); row.names(fleet.as.all)<-"#_fleet_assignment_to_allocation_group1"
fore1$fleet_assignment_to_allocation_group <-fore$fleet_assignment_to_allocation_group 
#---------------------------------------
#_if N allocation groups >0, list year, 
# allocation fraction for each 
# group  list sequentially because 
# read values fill to end of N forecast
# terminate with -9999 in year field 
fore1$N_allocation_groups <-1
#---------------------------------------
allocation <-data.frame(Year=2023,Group1=1); row.names(allocation)<-"#_allocation_among_groups1"
fore1$allocation_among_groups <-allocation
#---------------------------------------
# basis for input Fcast catch: 
# -1=read basis with each obs; 
# 2=dead catch; 
# 3=retained catch;
# 99=input Hrate(F)
fore1$InputBasis <-fore$InputBasis#2
fore1$eof <-TRUE

#===============================================================================
#   Escribir archivo modificado `forecast.ss`
#===============================================================================
r4ss::SS_writeforecast(mylist=fore1,
                       dir=dir.mod,
                       file="forecast.ss",
                       overwrite = TRUE,
                       verbose = TRUE)
#===============================================================================

