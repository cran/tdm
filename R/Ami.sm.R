Ami.sm<-function(a,b,c,d,e,f,g,h,i,j,k){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("AmiSMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Amidata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
tin=e,
D=f,
bw=g,
Ht=h,
Scr=i,
Gender=j,
age=k
)
, fileName=file.path(getwd(),"Amidata.txt"),digits=5)
modelData("Amidata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v","cl"))                                 # set monitored Pk paremeters
modelUpdate(10000)                                      # update 10000
}
