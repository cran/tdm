Amidihinfusion.ss<-function(a,b,f,g,h,i,j,k,m,e,x,y){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("AmidihinfusionSSmodel.txt")                       # load model
bugsData(                                                     # produce a BUGS data file and name it Amidihinfusiondata
list(
c=a,
T=b,
tinf=e,
R=m,
DL=k,
ht=f,
age=g,
smoke=h,
Gender=i,
CHF=j
)
, fileName=file.path(getwd(),"Amidihinfusiondata.txt"),digits=5)
modelData("Amidihinfusiondata.txt")                           # Load data
modelCompile(numChains=1)                                     # compile
modelGenInits()                                               # gen inits
modelUpdate(10000)                                             # burn in 4000
samplesSet(c("cl"))                                           # set monitored PK parameters
samplesSet(c("v"))                                            # set monitored PK parameters
modelUpdate(10000)                                            # update 10000
}

