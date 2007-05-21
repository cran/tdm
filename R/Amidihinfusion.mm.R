Amidihinfusion.mm<-function(a,i,b,c,g,h,e,j,k,L,m,n){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("AmidihinfusionMMmodel.txt")               # Load model
bugsData(                                             # porduce a BUGS data file and name it Amidihinfusiondata
list(T=a,
X=i,
c=c(b),
ts=c(c),
tinf=n,
R=m,
DL=L,
ht=g,
age=h,
smoke=e,
Gender=j,
CHF=k
)
, fileName=file.path(getwd(),"Amidihinfusiondata.txt"),digits=5)
modelData("Amidihinfusiondata.txt")                   # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(10000)                                     # burn in 4000
samplesSet(c("cl"))                                   # set monitored PK parameters
samplesSet(c("v"))                                    # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}

