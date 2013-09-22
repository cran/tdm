Car.ss<-function(a,b,c,d,e,f,g,h,i){
### library(BRugs)                                        # active BRugs
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                # set working directory
### modelCheck("CBZSSmodel.txt")                          # Load model
### bugsData(                                             # porduce a BUGS data file and name it Cardata
### list(
### c=a,
### tau=b,
### ts=c,
### D=d,
### TBW=e,
### PB=f,
### VPA=g,
### PHT=h,
### E=i
### )
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,   
tau=b, 
ts=c,  
D=d,   
TBW=e, 
PB=f,  
VPA=g, 
PHT=h, 
E=i    
)
### , fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
### modelData("CBZdata.txt")                              # Load data
### modelCompile(numChains=1)                             # compile
### modelGenInits()                                       # gen intis
### modelUpdate(4000)                                     # burn in 4000
### samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
### modelUpdate(10000)                                    # update 10000
params = c("cl_F" , "v_F")                                  # The parameter(s) to be monitored.
initsList = list(cl_F= 0.0734*((d/e/b*24)^0.406)*(e^0.694)*(1.45^h) *(1.17^f)*(1.21^g)*(0.849^i),
          v_F=1.91*e)                      # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "CBZSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
v_F.mat <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl_F.mat, v_F.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
cat("\n Estimated PK Parameters:\n")
cat("---------------------------\n")
show(params.mean)
cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])           ### for testing purpose here! -YJ
### v_F.mean <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                      ### Yes, it works now. --YJ
}


