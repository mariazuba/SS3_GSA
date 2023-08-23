#devtools::install_github('hafro/rgadget')

rm(list=ls())
graphics.off()

library(Rgadget)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# source("my_functions.R")

tmp<-callGadget(s=1,main='main',i='params.in',o='lik.out',ignore.stderr=FALSE, log='tmp')

# I apply a slightly modified function that I have created using the read.gadget.likelihood() function of Rgadget. The reason is that the class() function, in the current version of R that I am using (R4.2.0), I don´t knwo why, it produces a vector of two values, for example "matrix" "array", and this creates an error. I have modified it, so now it takes the first element of that vector of class objects.
# lik <- my_read.gadget.likelihood('likelihood')

lik <- read.gadget.likelihood('likelihood')

# Hay una función, read.gadget.file() que en principio es la que debe sustituir a read.gadget.likelihood, pero no devuelve un objeto con el mismo formato que read.gadget.likelihood() (que funciona con R3.6.3 y que en R4.2.0 la he adaptado para que coja el primer elemento devuelto con la función class() y no de error)
#lik <- read.gadget.file(path=getwd(), 'likelihood')

# opt=read.gadget.file(path=getwd(), file_name='optfile')
# opt$simann$simanniter <- 1
# opt$hooke$hookeiter <- 1
# opt$bfgs$bfgsiter <- 1
# write.gadget.file(opt, path=getwd())

# tmp<-gadget.iterative(params.file = 'params.in',
#                       optinfofile = 'optfile',
#                       wgts = 'WGTS',
#                       rew.sI=TRUE,
#                       cv.floor=0.05,
#                       grouping = list(sind=lik$surveyindices$name, seine = c('ldist.seine','ldist.alkseine')))


# tmp<-my_gadget.iterative(params.file = 'params.in',
#                          optinfofile = 'optfile',
#                          wgts = 'WGTS',
#                          rew.sI=TRUE,
#                          cv.floor=0.05,
#                          grouping = list(sind=lik$surveyindices$name, seine = c('ldist.seine','ldist.alkseine')))


# Hay nuevas funciones para sustituir a gadget.iterative, abajo una adaptación de lo que he encontrado en la ayuda de Rgadget
# gd <- gadget.variant.dir(getwd())
# tmp<-gadget_iterative_stage_1(gd,
#                               grouping = list(sind=lik$surveyindices$name, seine = c('ldist.seine','ldist.alkseine')),
#                               wgts = 'WGTS',
#                               params.in = 'params.in') %>% 
#   parallel::mclapply(gadget_optimize, mc.cores = parallel::detectCores()) %>% 
#   gadget_iterative_stage_2() %>% 
#   gadget_optimize()

# The function gadget.fit is deprecated, and in adition, some of the arguments have changed. For example, printfile.printatstart doesn´t exist anymore
# fit<-gadget.fit(wgts = "WGTS", printfile.printatstart = 0, printfile.steps = 2)
gd <- gadget.variant.dir(getwd())
fit<-gadget.fit(wgts = "WGTS", printatstart = 0, steps = 2, gd=gd)

# fit<-gadget_fit(gd, params.in="WGTS/params.final", steps = 2)


# st <- read.gadget.stockfiles('Modelfiles/anch')
st <- read.gadget.file(path=getwd() , 'Modelfiles/anch')

params <- read.gadget.parameters('WGTS/params.final')

rec <- Rgadget:::get.gadget.recruitment(st,params,collapse=FALSE)

SS<-read.gadget.lik.out('WGTS/lik.final')
# lik <- read.gadget.likelihood('likelihood')
lik.data<-read.gadget.data(lik)
save(params,rec,st,lik.data,SS,file="Output.Rdata")
source("gadget_fit4_2step_2019.r")

#Pasar al repositorio gadget-recovery correr en el cesga en el folder correspondiente#############################################################################33
library(icesTAF)
library(stringr)

path_results<-"/mnt/netapp2/Home_FT2/home/csic/epc/mrh/recovery-results-2020/"
#path_results<-"/run/user/1000/gvfs/sftp:host=ft3.cesga.es,user= csepcmrh/mnt/netapp2/Home_FT2/home/csic/epc/mrh/Gadget_2022/recovery-results-2020"
#sftp://csepcmrh@ft3.cesga.es/home/csic/epc/mrh/Gadget_2022
mkdir(paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file2<-c("Output.Rdata","resbyyear.Rdata", "optfile","params.in")
path_run<-getwd()
file.copy(paste(path_run,"/WGTS/WGTS.Rdata",sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[1],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[2],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[3],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[4],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))




















#fit<-gadget.fit(wgts = "WGTS_end", printfile.printatstart = 0, printfile.steps = 2)


#setwd("WGTS")
#F12<-data.frame(stock="anch",age.min=1,age.max=2)
#F1<-data.frame(stock="anch",age.min=1,age.max=1)
#F2<-data.frame(stock="anch",age.min=2,age.max=2)
#Files2copy<-list.files()
#list.files<-Files2copy[1:(length(Files2copy)-1)]

#dir.create("../WGTS1")
#dir.create("../WGTS2")
#dir.create("../WGTS12")
#dir.create("../WGTSseine")
#dir.create("../WGTSpelago")
#dir.create("../WGTSecocadiz")

#HIce WGTS1 a mano en el 44porque necesita casi todos los archivos
#file.copy(from=list.files, to = "../WGTS1")
#file.copy(from=list.files, to = "../WGTS2")
# file.copy(from=list.files, to = "../WGTS12")
# file.copy(from=list.files, to = "../WGTSseine")
# file.copy(from=list.files, to = "../WGTSpelago")
# file.copy(from=list.files, to = "../WGTSecocadiz")

#setwd("..")
#fit1 <- gadget.fit(wgts = 'WGTS1', 
                  # f.age.range=F1)

#fit2 <- gadget.fit(wgts = 'WGTS2', 
                  # f.age.range=F2)
#F12<-data.frame(stock="anch",age.min=1,age.max=2)
#fit12 <- gadget.fit(wgts = 'WGTS12', 
                    #f.age.range=F12)





#retro<-gadget.retro(mainfile = 'WGTS/main.final', iterative=TRUE)
