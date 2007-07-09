Val.sm<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ValSMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Valdata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
age=f,
CBZ=g,
INDI=h,
ka=i
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("cl"))                                     # set monitored Pk parameters
modelUpdate(10000)                                      # update 10000
}



