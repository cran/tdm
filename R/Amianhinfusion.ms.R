Amianhinfusion.ms<-function(a,b,c,g,h,i,j,k,L,m,n,x,y){
library(BRugs)                                                     # active BRugs         
oldwd<-getwd()                                                         
setwd(system.file("PK",package="tdm"))                             # set working directory
modelCheck("AmianhinfusionMSmodel.txt")                            # Load model
bugsData(                                                          # produce a BUGS data file and name it Amianhinfusiondata
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
, fileName=file.path(getwd(),"Amianhinfusiondata.txt"),digits=5)
modelData("Amianhinfusiondata.txt")                                # Load data
modelCompile(numChains=1)                                          # compile
modelGenInits()                                                    # gen inits
modelUpdate(10000)                                                  # burn in 4000
samplesSet(c("cl"))                                                # set monitored PK parameter
samplesSet(c("v"))                                                 # set monitored PK parameter
modelUpdate(10000)                                                 # update 10000
}

