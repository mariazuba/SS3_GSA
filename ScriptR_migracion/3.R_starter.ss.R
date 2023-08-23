##############################################################################################
### Migración de a4a a SS3 ----
##############################################################################################
library(here)
# CARGAR LIBRERIAS ----
source(here("ScriptR_migracion","Librerias_Requeridas.R")) 

# CARGAR DIRECTORIOS ----
source(here("ScriptR_migracion","Directorios_Requeridos.R")) 
#===============================================================================
#
# Leer archivo starter.ss ----
#
#===============================================================================
inputs <- r4ss::SS_read(dir = dir.base)
start<-inputs$start # archivo base

#===============================================================================
#
# Modificar archivo starter.ss  ----
#
#===============================================================================

start1<-start # archivo modificado

#===============================================================================
# Especificaciones  ----
#===============================================================================
start1$sourcefile             <- start$sourcefile # no se modifica
start1$type                   <- start$type       # no se modifica
start1$SSversion              <- start$SSversion  # no se modifica
start1$datfile                <- "data_GSA6.ss"    # file name of the data file, opción requerida obligatoria
start1$ctlfile                <- "control_GSA6.ss" # file name of the control file, opción requerida obligatoria
start1$init_values_src        <- start$init_values_src #0= use values in control file; 
start1$run_display_detail     <- start$run_display_detail #0 = none other than ADMB outputs
start1$detailed_age_structure <- start$detailed_age_structure #1 = include all output (with wtatage.ss_new), full Report file.
start1$checkup                <- start$checkup #0=omit; because is mostly used by the developer
start1$parmtrace              <- start$parmtrace #0=omit file parmtrace.sso
start1$cumreport              <- start$cumreport #0=omit file Cumreport.sso
start1$prior_like             <- start$prior_like #1=calculate priors for all parameters that have a defined prior 
start1$soft_bounds            <- start$soft_bounds #1=use, this option creates a weak symmetric beta penalty for the selectivity parameters.
start1$N_bootstraps           <- start$N_bootstraps #1=output an annotated replicate of the input data file (number of data file to output)
start1$last_estimation_phase  <- start$last_estimation_phase #10=exit after completing 10 phase (turn off estimation)
start1$MCMCburn               <- start$MCMCburn #0=number of iteration to discard at the start of an MCMC run
start1$MCMCthin               <- start$MCMCthin #1=number of iteration to remove between the main period of the MCMC run
start1$jitter_fraction        <- start$jitter_fraction #0 =no jitter done to starting values
start1$minyr_sdreport         <- -1 # -1=begin anual SD report in start  year 
start1$maxyr_sdreport         <- -1 # -1=end annual SD report in end year
start1$N_STD_yrs              <- start$N_STD_yrs #0 = none extra SD Report Years
start1$converge_criterion     <- start$converge_criterion #1e-05 = this is a reasonable default value for the change in log likelihood denoting convergence.
start1$retro_yr               <- start$retro_yr #0 = none; adjusts the model end year and disregards data after this year. May not handle time varying parameters completely
start1$min_age_summary_bio    <- 0 # 0=minimum integer age for inclusion in the summary biomass used for reporting and for calculation of total explotation rate
start1$depl_basis             <- start$depl_basis #2 = X*SBmsy; Selects the basis for the denominator when calculating degree of depletion in SB. The calculated values are reported to the SD report.
start1$depl_denom_frac        <- start$depl_denom_frac #1 = fraction (X) for depletion denominator. Value for use in the calculation of the ratio for SBy/(X*SB0)
start1$SPR_basis              <- start$SPR_basis #4 = no denominator, so report actual 1-SPR values
start1$F_report_units         <- start$F_report_units # 3 = sum(apical F's by fleet)
start1$F_age_range            <- start$F_age_range # NA NA = Specify age range if F std reporting >= 4
start1$F_report_basis         <- start$F_report_basis #0 = not relative, report raw values; Selects the denominator to use when reporting the Fstd report values.
start1$MCMC_output_detail     <- start$MCMC_output_detail #0 = default; specify format of MCMC output
start1$ALK_tolerance          <- start$ALK_tolerance #0 >=value required to Age-lenght-key (ALK) tolerance level 
start1$final                  <- start$final #3.3 = Model version check value
start1$seed                   <- start$seed #-1 = seed value, specify a seed for data generation (optional)

#===============================================================================
#   Escribir archivo modificado `starter.ss`
#===============================================================================
r4ss::SS_writestarter(mylist=start1,
                      dir=dir.mod,
                      file="starter.ss",
                      overwrite = TRUE,
                      verbose = TRUE)
#===============================================================================

