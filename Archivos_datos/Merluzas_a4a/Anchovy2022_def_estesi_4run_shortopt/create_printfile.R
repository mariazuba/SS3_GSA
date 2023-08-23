
rm(list=ls())

library(Rgadget)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



gd=gadget.variant.dir(getwd())

# make.gadget.printfile(main.file="main", file="printfile", printatstart=1, steps=1, gd=list(getwd(), output = "output", aggfiles = "Aggfiles"))
# make.gadget.printfile() # Ya no existe esta función

# printfil=gadgetprintfile(file_name="printfile", path=getwd(), missingOkay=TRUE)
# write.gadget.file(printfil, path=getwd())
# aver <- gadget_stockprinter_component(
#   printfile="stockprinter_prueba",
#   stocknames="anch",
#   area="Aggfiles/allarea.agg",
#   age="Aggfiles/allages.agg",
#   len="Aggfiles/alllen.agg",
#   precision=3,
#   printatstart=1,
#   yearsandsteps="all 1"                
# )
# write.gadget.file(aver, path=getwd())


printfil=gadgetprintfile(file_name="printfile", path=getwd(), missingOkay=TRUE) %>%
  gadget_update("stockprinter",
                printfile="stockprinter_prueba",
                stocknames="anch",
                area="Aggfiles/allarea.agg",
                age="Aggfiles/allages.agg",
                len="Aggfiles/alllen.agg",
                precision=3,
                printatstart=1,
                yearsandsteps="all 1") %>%
write.gadget.file(., path=getwd())


fit <- gadget.fit()


gadget.fit()

gadget.fit(
  wgts = "WGTS",
  main.file = "main",
  fleet.predict = NULL,
  mat.par = NULL,
  params.file = NULL,
  f.age.range = NULL,
  fit.folder = "output",
  printatstart = 1,
  steps = 1,
  recruitment_step_age = NULL,
  gd = NULL
)



load("Output.Rdata")


