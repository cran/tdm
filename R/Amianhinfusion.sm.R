Amianhinfusion.sm<-function(a,b,c,g,h,i,j,k,L,m,n){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("AmianhinfusionSMmodel.txt")               # Load model
bugsData(                                             # porduce a BUGS data file and name it Amianhinfusiondata
list(T=a,
c=c(b),
ts=c(c),
tinf=n,
R=m,
DL=L,
ht=g,
age=h,
smoke=i,
Gender=j,
CHF=k
)
, fileName=file.path(getwd(),"Amianhinfusiondata.txt"),digits=5)
modelData("Amianhinfusiondata.txt")                   # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(10000)                                     # burn in 4000
samplesSet(c("cl"))                                   # set monitored PK parameters
samplesSet(c("v"))                                    # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}

