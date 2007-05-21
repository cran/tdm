Cyc.ss<-function(a,b,c,d,e,f,g){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("CycSSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Cycdata
list(
c=a,
PTD=b,
tau=c,
ts=d,
D=e,
bw=f,
Dia=g
)
, fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
modelData("Cycdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored Pk parameters
modelUpdate(10000)                                      # update 10000
}



