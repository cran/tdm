Ind.ss<-function(a,b,c,d,e,f,g){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("IndSSmodel.txt")                                  # load model
bugsData(                                                     # produce a BUGS data file and name it Inddata
list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
Rit=f,
Gender=g
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")                                      # Load data
modelCompile(numChains=1)                                     # compile
modelGenInits()                                               # gen inits
modelUpdate(10000)                                            # burn in 25000
samplesSet(c("ka","cl_F"))                                    # set monitored PK parameters
modelUpdate(60000)                                            # update 50000
}



