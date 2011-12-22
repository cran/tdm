Amidihinfusion.ms<-function(a,b,c,g,h,i,j,k,L,m,n,x,y){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("AmidihinfusionMSmodel.txt")                       # load model
bugsData(                                                     # produce a BUGS data file and name it Amidihinfusiondata
list(N=a,
c=c(b),
T=c(c),
tinf=c(n),
R=c(m),
DL=c(L),
ht=c(g),
age=c(h),
smoke=c(i),
Gender=c(j),
CHF=c(k)
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
