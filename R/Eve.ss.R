Eve.ss<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("EveSSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Evedata
list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
age=f,
race=g,
Ery=h
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v_F","cl_F"))                             # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}

