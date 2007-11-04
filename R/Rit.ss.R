Rit.ss<-function(a,b,c,d,e){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("RitSSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Ritdata
list(
c=a,
ts=b,
tau=c,
D=d,
LPV=e
)
, fileName=file.path(getwd(),"Ritdata.txt"),digits=5)
modelData("Ritdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F","ka"))                      # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


