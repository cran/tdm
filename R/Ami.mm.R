Ami.mm<-function(a,b,C,d,e,f,g,h,l,J,k,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("AmiMMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Amidata
list(T=a,
X=i,
c=c(b),
ts=c(C),
tau=d,
tin=e,
D=f,
bw=g,
Ht=h,
Scr=l,
Gender=J,
age=k
)
, fileName=file.path(getwd(),"Amidata.txt"),digits=5)
modelData("Amidata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v","cl"))                                 # set monitored Pk parameters
modelUpdate(10000)                                      # update 10000
}
