Car.ms<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("CBZMSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Cardata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
TBW=c(f),
PB=c(g),
VPA=c(h),
PHT=c(i),
E=c(j)
)
, fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
modelData("CBZdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


