ChiVal.ms<-function(a,b,c,d,e,f,g){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ChiValMSmodel.txt")                         # Load model
bugsData(                                               # produce a BUGS data file and name it ChiValdata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
TBW=c(f),
CBZ=c(g)
)
, fileName=file.path(getwd(),"ChiValdata.txt"),digits=5)
modelData("ChiValdata.txt")                             # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitpred PK parameters
modelUpdate(10000)                                      # update 10000
}



