Eno.ss<-function(a,b,c,d,e,f){                         
library(BRugs)                                          # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("EnoSSmodel.txt")                            # model check                  
bugsData(                                               # produce a BUGS data file and name it Enodata
list(
Amax=a,
tau=b,
D=c,
TBW=d,
Gender=e,
Scr=f
)
, fileName=file.path(getwd(),"Enodata.txt"),digits=5)
modelData("Enodata.txt")                               # Load data file
modelCompile(numChains=1)                              # compile 
modelGenInits()                                        # gen inits
modelUpdate(4000)                                      # burn in 4000
samplesSet(c("v_F","cl_F"))                            # set PK parameters monitoring
modelUpdate(10000)                                     # update more 10000
}

