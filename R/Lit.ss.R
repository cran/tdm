Lit.ss<-function(a,b,c,d,e,f,g){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("LitSSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Litdata
list(
c=a,
tau=b,
age=c,
D=d,
Scr=e,
bw=f,
f=g
)
, fileName=file.path(getwd(),"Litdata.txt"),digits=5)
modelData("Litdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


