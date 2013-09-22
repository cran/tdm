###
### the 1st BUGS model which was tried to be re-modeled with JAGS. [2013/6/24 am 03:04:55] -YJ
###
Phe.ss<-function(a,b,c,d){
options(digits=5)                                          # remark this after testing. -YJ
## library(BRugs)                                          # activate BRugs; for BUGS only
## require(rjags)                                          # activate rjags here; or 'library(rjags)' will work too.
## library(coda)
library(R2jags)                                          # not used right now; may be later. --YJ
### oldwd<-getwd()                                          # with jags, do not change wd here! -YJ
### setwd(system.file("PK",package="tdm"))                  # set working directory; do not change wd here! -YJ
### modelCheck("PheSSmodel.txt")                            # Load model; for BUGS only
### bugsData(                                               # produce a BUGS data file and name it Phedata
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,
tau=b,
D=c,
bw=d
)

### , fileName=file.path(getwd(),"Phedata.txt"),digits=5)   # marked when using JAGS
### modelData("Phedata.txt")                                # Load data; for BUGS only
###
### the following two commands can be done automatically in jags.model() by commenting out inits argument.
###
### modelCompile(numChains=1)                               # compile; for BUGS only
### modelGenInits()                                         # gen inits; for BUGS only
### modelUpdate(6000)                                       # burn in 6000; for BUGS only
### samplesSet(c("Vmax","Km"))                              # set monitored PK parameters; for BUGS only
### modelUpdate(506000)                                     # update 50000; for BUGS only
### 
### the following lines are all coded based on JAGS modeling. -YJ
###
params = c("Vmax" , "Km")                                   # The parameter(s) to be monitored.
initsList = list(Vmax=7.22*d, Km=4.44)                      # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "PheSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### codaSamples <- autojags(jagsModel, params, n.iter=nIter)              ### still not work!  figure out how. -YJ 

# resulting codaSamples object has these indices:
# codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS
# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
###
### start here to avoid logging too many progress bar from JAGS!
###
### zz<- file("phenytoin_tdm.txt",open="wt")
### sink(zz, split=TRUE)

checkConvergence = TRUE
if (checkConvergence) {
  ### openGraph(width=7,height=7)   ### there is a openGraphSaveGraph.R file; not to use for now.
  show(summary(codaSamples))
  ### str(codaSamples)
  ### show(gelman.diag(codaSamples))
  ### effectiveChainLength = effectiveSize(codaSamples) 
  ### show(effectiveChainLength)
  dev.new()
  plot(codaSamples) 
  ### dev.new()
  ### autocorr.plot(codaSamples)
  ### dev.new()
  ### caterplot(codaSamples)
  ### dev.new()
  ### traplot(codaSamples)
  dev.new()
  gelman.plot(codaSamples)               ### works now.  -YJ
  ### dev.new()
  ### denplot(codaSamples)               ### only with mcmcplot() package
  # dev.new()
  # traceplot(codaSamples)
  ### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ
}
###
### show prediction/calc Cp here
###
cat("\n")
Km.mat <- as.matrix(codaSamples[[1]])
Vmax.mat <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(Km.mat, Vmax.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
cat("\n Estimated PK Parameters:\n")
cat("---------------------------\n")
show(params.mean)
cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### Km.mean <- as.matrix(params.mean[[1]])             ### for testing purpose here! -YJ
### Vmax.mean <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                      ### Yes, it works now. --YJ

####################################################### the write & read is OK now. #############################
# X <- read.table("params.csv",header=FALSE)
# unlink("params.csv")                                   ### great & be careful. this will delete the file!  --YJ
# show(X)
###
### it works fine now. -YJ
###
# km.read   <- X[1,2]
# vmax.read <- X[2,2]
# cat("\n show Km & Vmax read back from params.csv:\n")
# show(km.read);show(vmax.read);cat("\n")
#################################################################################################################
Km.mean   <- as.matrix(params.mean[[1]])
Vmax.mean <- as.matrix(params.mean[[2]])
cat("\n")
### where b = tau, c = Dose; a = obs. Cp               ### in the way, it works.
  Cp.pred<-(c*24/b)*(Km.mean)/(Vmax.mean-c*24/b)       ###  how to extract final results from JAGS?  -YJ
  Final<-c(Cp.pred, a)                                 ###  <- how to extract final parameter values here?  -YJ
  coutput<-data.frame(Final)
  row.names(coutput)<-list("C(ss_calc)","C(ss_obs)")
  show(coutput)
  cat("\n\n")
}
