Rit.ms<-function(a,b,c,d,e,f){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("RitMSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Ritdata
list(N=a,
c=c(b),
ts=c(c),
tau=c(d),
D=c(e),
LPV=c(f)
)
, fileName=file.path(getwd(),"Ritdata.txt"),digits=5)
modelData("Ritdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F","ka"))                      # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


