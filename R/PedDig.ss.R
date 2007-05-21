PedDig.ss<-function(a,b,c,d,e){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("PedDigSSmodel.txt")                       # Load model
bugsData(                                             # porduce a BUGS data file and name it PedDigdata
list(
c=a,
tau=b,
D=c,
bw=d,
age=e
)
, fileName=file.path(getwd(),"PedDigdata.txt"),digits=5)
modelData("PedDigdata.txt")                           # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


