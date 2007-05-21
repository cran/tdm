Van.ss<-function(a,b,c,d,e,f,g,h,i,x,y){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("VanSSmodel.txt")                                  # load model
bugsData(                                                     # produce a BUGS data file and name it Vandata
list(
c=a,
tau=b,
ts=c,
tin=d,
D=e,
bw=f,
Scr=g,
Gender=h,
age=i
)
, fileName=file.path(getwd(),"Vandata.txt"),digits=5)
modelData("Vandata.txt")                                      # Load data
modelCompile(numChains=1)                                     # compile
modelGenInits()                                               # gen inits
modelUpdate(4000)                                             # burn in 4000
samplesSet(c("cl","v"))                                       # set monitored PK parameters
modelUpdate(10000)                                            # update 10000
}



