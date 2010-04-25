Van.ms<-function(a,b,c,d,e,f,g,h,i,j,x,y){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("VanMSmodel.txt")                                  # load model
bugsData(                                                     # produce a BUGS data file and name it Vandata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
tin=c(e),
D=c(f),
bw=c(g),
Scr=c(h),
Gender=c(i),
age=c(j)
)
, fileName=file.path(getwd(),"Vandata.txt"),digits=5)
modelData("Vandata.txt")                                      # Load data
modelCompile(numChains=1)                                     # compile
modelGenInits()                                               # gen inits
modelUpdate(4000)                                             # burn in 4000
samplesSet(c("cl","v"))                                       # set monitored PK parameters
modelUpdate(10000)                                            # update 10000
}



