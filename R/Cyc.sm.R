Cyc.sm<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("CycSMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Cycdata
list(T=a,
c=c(b),
ts=c(c),
PTD=d,
tau=e,
D=f,
bw=g,
Dia=h
)
, fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
modelData("Cycdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitoed Pk parameters
modelUpdate(10000)                                      # update 10000
}



