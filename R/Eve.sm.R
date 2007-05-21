Eve.sm<-function(a,b,C,d,e,f,g,h,j){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("EveSMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Evedata
list(T=a,
c=c(b),
ts=c(C),
tau=d,
D=e,
bw=f,
age=g,
race=h,
Ery=j
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v_F","cl_F"))                             # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}

