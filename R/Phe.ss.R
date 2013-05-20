Phe.ss<-function(a,b,c,d){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("PheSSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Phedata
list(
c=a,
tau=b,
D=c,
bw=d
)
, fileName=file.path(getwd(),"Phedata.txt"),digits=5)
modelData("Phedata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(6000)                                       # burn in 4000
samplesSet(c("Vmax","Km"))                              # set monitored PK parameters
modelUpdate(506000)                                     # update 50000
}


