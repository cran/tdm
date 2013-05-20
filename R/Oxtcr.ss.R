Oxtcr.ss<-function(a,b,d,e,f,g,h,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("OxtcrSSmodel.txt")                        # Load model
bugsData(                                             # porduce a BUGS data file and name it Oxtcrdata
list(
c=a,
tau=b,
D=d,
ht=e,
age=f,
smoke=g,
Gender=h,
CHF=i
)
, fileName=file.path(getwd(),"Oxtcrdata.txt"),digits=5)
modelData("Oxtcrdata.txt")                            # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}

