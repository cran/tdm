Car.mm<-function(J,A,B,C,d,e,F,G,H,K,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("CBZMMmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Cardata
list(T=J,
c=c(A),
ts=c(B),
tau=C,
D=d,
TBW=e,
PB=F,
VPA=G,
PHT=H,
E=K,
X=i
)
, fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
modelData("CBZdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


