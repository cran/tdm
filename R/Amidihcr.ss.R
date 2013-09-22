Amidihcr.ss<-function(a,b,d,e,f,g,h,i){
### ### library(BRugs)                                        # active BRugs
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                # set working directory
### modelCheck("AmidihcrSSmodel.txt")                     # Load model
### bugsData(                                             # porduce a BUGS data file and name it Amidihcrdata
### list(
### c=a,
### tau=b,
### D=d,
### ht=e,
### age=f,
### smoke=g,
### Gender=h,
### CHF=i
### )
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,
tau=b,  ### no 'c' here for quick coding?  -YJ
D=d,
ht=e,      
age=f,     
smoke=g,   
Gender=h,  
CHF=i      
)
### , fileName=file.path(getwd(),"Amidihcrdata.txt"),digits=5)
### modelData("Amidihcrdata.txt")                         # Load data
### modelCompile(numChains=1)                             # compile
### modelGenInits()                                       # gen intis
### modelUpdate(4000)                                     # burn in 4000
### samplesSet(c("cl_F"))                                 # set monitored PK parameters
### modelUpdate(10000)                                    # update 10000
IBW<-h*(50+(2.3*(e/2.4-60)))+(1-h)*(45+(2.3*(e/2.4-60)))
params = c("cl_F")                                        # The parameter(s) to be monitored.
initsList = list(cl_F=(0.037*IBW-0.006*f)*(1.284^g)*(0.751^i))  # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "AmidihcrSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
  gelman.plot(codaSamples)             ### why is this line not working?  -YJ
  # dev.new()
  # denplot(codaSamples)
  # dev.new()
  # traceplot(codaSamples)
  ### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ
}
###
### show prediction/calc Cp obtained from JAGS here
###
cat("\n")
cl_F.mat <- as.matrix(codaSamples[[1]])
posterior.estimates <- rbind(cl_F.mat)
vars <- t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
### cat("\n Estimated PK Parameters:\n")
### cat("---------------------------\n")
### show(params.mean)
### cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])               ### for testing purpose here! -YJ
### v_F.mean <- as.matrix(params.mean[[2]])
### half_life.mean <-log(2)/(cl_F.mean/vd_F.mean)          ### secondary PK param.  -YJ
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now. -YJ
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
X <- data.frame(Estimated_Parameters=c("Cl/F"),value=c(X[1,2]))
show(X);cat("\n\n")
}

