
# MODELO SS3 HACKE_GSA6_BYSEX MARZO 2025

library(r4ss)
library(tidyverse)
library(lubridate)
library(ss3diags)

# Primero descargo SS3 para mac

#r4ss::get_ss3_exe("HAKE_GSA6_BYSEX", version = "v3.30.23.1") # BUSCAR ULTIMA VERSION

#'*------------------------------------------------------------------------------------------*
# Luego ejecuto el modelo
old_wd <- getwd()
wd<-paste0(old_wd,"/HAKE_GSA6_BYSEX")
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)
setwd(old_wd)


# miro los resultados

# read output SS3
output <- r4ss::SS_output(dir = wd,forecast=FALSE)
# summary SS3
summary <- read.table(paste0(wd,"/ss_summary.sso"),header=F,sep="",na="NA",fill=T)

# Create the standard ss3 plots ----
r4ss::SS_plots(replist = output, dir = wd,
               printfolder = "plots",showpost = FALSE)


convergency<-output$maximum_gradient_component
like<-output$likelihoods_used

# run_cpue<-SSplotRunstest(output,subplots = "cpue")
# jaba_cpue<-SSplotJABBAres(output,subplots = "cpue")
# run_age<-SSplotRunstest(output,subplots = "age")
# jaba_age<-SSplotJABBAres(output,subplots = "age")
# 


