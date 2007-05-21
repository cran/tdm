Car.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("CBZSSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Cardata
list(
c=a,
tau=b,
ts=c,
D=d,
TBW=e,
PB=f,
VPA=g,
PHT=h,
E=i
)
, fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
modelData("CBZdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


