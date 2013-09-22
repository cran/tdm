Dig.ss<-function(a,b,c,d,e,f,g){
### library(BRugs)                                        # active BRugs
options(digits=5)                                          # remark this after testing. -YJ
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                # set working directory
### modelCheck("DigSSmodel.txt")                          # Load model
### bugsData(                                             # produce a BUGS data file and name it Digdata
### list(
### c=a,
### tau=b,
### D=c,
### scr=d,
### bw=e,
### age=f,
### Gender=g
### )
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,    
tau=b,  
D=c,    
scr=d,  
bw=e,   
age=f,  
Gender=g
)
### , fileName=file.path(getwd(),"Digdata.txt"),digits=5)
### modelData("Digdata.txt")                              # Load data
### modelCompile(numChains=1)                             # compile
### modelGenInits()                                       # gen intis
### modelUpdate(4000)                                     # burn in 4000
### samplesSet(c("cl_F"))                                 # set monitored PK parameters
### modelUpdate(10000)                                    # update 10000
params = c("cl_F")                                        # The parameter(s) to be monitored.
initsList = list(cl_F=(8.03/24)*(1-0.0058*f)*e*(d^(-0.6))*(0.88^(1-g))) 
                                                            # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "DigSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
  dev.new()
  autocorr.plot(codaSamples)
  ### dev.new()
  ### caterplot(codaSamples)
  ### dev.new()
  ### traplot(codaSamples)
  dev.new()
  gelman.plot(codaSamples)             ### why is this line not working?  -YJ
  # dev.new()
  # denplot(codaSamples)
  # dev.new()
  # traceplot(codaSamples)
  ### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ
}
###
### show prediction/calc Cp here
###
cat("\n")
cl_F.mat <- as.matrix(codaSamples[[1]])
posterior.estimates <- rbind(cl_F.mat)    ### don't know if it works or not for singel PK parameter? -YJ
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
cat("\n Estimated PK Parameters:\n")
cat("---------------------------\n")
show(params.mean)
cat("\n\n")
### cl_F.mean <- as.matrix(params.mean[[1]])           ### for testing purpose here! -YJ
### show(Km.mean);show(Vmax.mean)                      ### Yes, it works now. --YJ
write.table(params.mean,file="params.csv",col.names=FALSE)
}


