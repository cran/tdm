Cyc.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("CycMSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Cycdata
list(N=a,
c=c(b),
PTD=c(c),
tau=c(d),
ts=c(e),
D=c(f),
bw=c(g),
Dia=c(h)
)
, fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
modelData("Cycdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored Pk parameters
modelUpdate(10000)                                      # update 10000
}



