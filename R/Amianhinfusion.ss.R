Amianhinfusion.ss<-function(a,b,f,g,h,i,j,k,m,e,x,y){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("AmianhinfusionSSmodel.txt")                   # model check
bugsData(                                                 # produce a BUGS data file and name it Amianhinfusiondata
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
, fileName=file.path(getwd(),"Amianhinfusiondata.txt"),digits=5)
modelData("Amianhinfusiondata.txt")                       # Load data
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen inits
modelUpdate(10000)                                         # burn in 4000
samplesSet(c("cl"))                                       # set monitored PK parameter
samplesSet(c("v"))                                        # set monitored PK parameter
modelUpdate(10000)                                        # update 10000
}

