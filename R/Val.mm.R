Val.mm<-function(k,A,B,d,e,f,g,h,J,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ValMMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Valdata
list(T=k,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
age=c(f),
CBZ=c(g),
INDI=c(h),
ka=c(J)
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}



