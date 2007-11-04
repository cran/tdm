Lit.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("LitMSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Litdata
list(N=a,
c=c(b),
tau=c(c),
age=c(d),
D=c(e),
Scr=c(f),
bw=c(g),
f=c(h)
)
, fileName=file.path(getwd(),"Litdata.txt"),digits=5)
modelData("Litdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}



