Phe.ms<-function(a,b,c,d,e){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("PheMSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Phedata
list(N=a,
c=c(b),
tau=c(c),
D=c(d),
bw=c(e)
)
, fileName=file.path(getwd(),"Phedata.txt"),digits=5)
modelData("Phedata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # buurn in 4000
samplesSet(c("Vmax","Km"))                              # set monitored PK parameters
modelUpdate(50000)                                      # update 50000
}


