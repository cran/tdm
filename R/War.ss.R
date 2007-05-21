War.ss<-function(a,b,c){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("WarSSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Wardata
list(
D=a,
tau=b,
INR=c
)
, fileName=file.path(getwd(),"Wardata.txt"),digits=5)
modelData("Wardata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(25000)                                    # burn in 4000
samplesSet(c("Cpmax","cl_F","kc","m","v_F"))          # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


