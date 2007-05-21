PedDig.ms<-function(a,b,c,d,e,f){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("PedDigMSmodel.txt")                       # Load model
bugsData(                                             # porduce a BUGS data file and name it PedDigdata
list(N=a,
c=c(b),
tau=c(c),
D=c(d),
bw=c(e),
age=c(f)
)
, fileName=file.path(getwd(),"PedDigdata.txt"),digits=5)
modelData("PedDigdata.txt")                           # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


