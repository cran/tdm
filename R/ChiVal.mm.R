ChiVal.mm<-function(k,A,B,d,e,f,g,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ChiValMMmodel.txt")                         # Load model
bugsData(                                               # produce a BUGS data file and name it ChiValdata
list(T=k,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
TBW=c(f),
CBZ=c(g)
)
, fileName=file.path(getwd(),"ChiValdata.txt"),digits=5)
modelData("ChiValdata.txt")                             # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored PK parameter
modelUpdate(10000)                                      # update 10000
}



