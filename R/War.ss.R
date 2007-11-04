War.ss<-function(a,b,c){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("WarSSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Wardata
list(
D=a,
tau=b,
INR=c
)
, fileName=file.path(getwd(),"Wardata.txt"),digits=5)
modelData("Wardata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
bugsData(
list(
m=c(0.422),
Cpmax=c(5.49),
kc=c(0.98),
cl_F=c(2.19),
v_F=c(7.5)
)
, fileName=file.path(getwd(),"WarSSInits.txt"),digits=5)
modelInits("WarSSInits.txt")
modelUpdate(4000)                                    # burn in 4000
samplesSet(c("Cpmax","cl_F","kc","m","v_F"))          # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


