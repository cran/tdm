Dig.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("DigMSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Digdata
list(N=a,
c=c(b),
tau=c(c),
D=c(d),
scr=c(e),
bw=c(f),
age=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Digdata.txt"),digits=5)
modelData("Digdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


