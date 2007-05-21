Their.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("TheirSSmodel.txt")                            # model check
bugsData(                                                 # produce a BUGS data file and name it Theirdata
list(
c=a,
tau=b,
ts=c,
D=d,
ht=e,
age=f,
smoke=g,
Gender=h,
CHF=i
)
, fileName=file.path(getwd(),"Theirdata.txt"),digits=5)
modelData("Theirdata.txt")                                # Load data
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen intis
modelUpdate(4000)                                         # burn in 4000
samplesSet(c("v_F","cl_F"))                               # set monitored PK parameters
modelUpdate(30000)                                        # update 30000
}

