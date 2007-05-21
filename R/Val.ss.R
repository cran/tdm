Val.ss<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ValSSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Valdata
list(
c=a,
tau=b,
ts=c,
D=d,
age=e,
CBZ=f,
INDI=g,
ka=h
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits 
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}



