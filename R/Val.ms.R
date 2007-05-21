Val.ms<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ValMSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Valdata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
age=c(f),
CBZ=c(g),
INDI=c(h),
ka=c(i)
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored Pk paremeters
modelUpdate(10000)                                      # update 10000
}



