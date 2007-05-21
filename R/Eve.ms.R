Eve.ms<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("EveMSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Evedata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
bw=c(f),
age=c(g),
race=c(h),
Ery=c(i)
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v_F","cl_F"))                             # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}

