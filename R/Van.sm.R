Van.sm<-function(a,b,c,d,e,f,g,h,i,j,x,y){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("VanSMmodel.txt")                                  # load model
bugsData(                                                     # produce a BUGS data file and name it Vandata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
tin=e,
D=f,
bw=g,
Scr=h,
Gender=i,
age=j
)
, fileName=file.path(getwd(),"Vandata.txt"),digits=5)
modelData("Vandata.txt")                                      # Load data
modelCompile(numChains=1)                                     # compile
modelGenInits()                                               # gen inits
modelUpdate(4000)                                             # burn in 4000
samplesSet(c("cl","v"))                                       # set monitored PK parameters
modelUpdate(10000)                                            # update 10000
}



