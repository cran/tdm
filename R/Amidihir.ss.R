Amidihir.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("AmidihirSSmodel.txt")                       # Load model
bugsData(                                               # produce a BUGS data file and name it Amidihirdata
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
, fileName=file.path(getwd(),"Amidihirdata.txt"),digits=5)
modelData("Amidihirdata.txt")                                # Load data
modelCompile(numChains=1)                                    # compile
modelGenInits()                                              # gen inits
modelUpdate(4000)                                            # burn in 4000
samplesSet(c("v_F","cl_F"))                                  # set monitored PK paremeters
modelUpdate(30000)                                           # update 30000
}

