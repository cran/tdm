Amianhcr.ss<-function(a,b,d,e,f,g,h,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("AmianhcrSSmodel.txt")                     # Load model
bugsData(                                             # porduce a BUGS data file and name it Amianhcrdata
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
, fileName=file.path(getwd(),"Amianhcrdata.txt"),digits=5)
modelData("Amianhcrdata.txt")                         # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}

