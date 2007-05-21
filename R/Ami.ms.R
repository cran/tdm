Ami.ms<-function(a,b,c,d,e,f,g,h,i,j,k){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("AmiMSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Amidata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
tin=c(e),
D=c(f),
bw=c(g),
Ht=c(h),
Scr=c(i),
Gender=c(j),
age=c(k)
)
, fileName=file.path(getwd(),"Amidata.txt"),digits=5)
modelData("Amidata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v","cl"))                                 # set monitored Pk parameters
modelUpdate(10000)                                      # update 10000
}
