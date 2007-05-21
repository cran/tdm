Ami.ss<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("AmiSSmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Amidata
list(
c=a,
tau=b,
ts=c,
tin=d,
D=e,
bw=f,
Ht=g,
Scr=h,
Gender=i,
age=j
)
, fileName=file.path(getwd(),"Amidata.txt"),digits=5)
modelData("Amidata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v","cl"))                                 # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}

